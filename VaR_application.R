### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(lubridate)
  library(magrittr)
  library(tidyr)
  library(tictoc)
  library(data.table)
  library(highfrequency)
  library(ggplot2)
  library(xts)
  library(stringi)
  library(Rsolnp)
  library(viridis)
  library(RColorBrewer)
  library(gridExtra)
})

# source("helpfuns.R")
# source("get_RC.R")
### Functions -----------------------------------------------------------------
portfolio_strategy <- function(
  daily_return,
  intraday_refreshed, 
  start_date = "2023-10-02"
) {
  
  mu_t <- daily_return %>% 
    dplyr::filter(date < lubridate::as_date("2023-10-02")) %>% 
    dplyr::select(-date) %>% 
    as.matrix() %>% 
    colMeans()
  
  RC_list <- get_daily_RC(intraday_refreshed)
  
  get_weights <- function(Date, daily_return, intraday_refreshed, RC_list, mu_t) {
    VaR <- function(omega, sigma, alpha, mu_t) {
      res <- -t(omega)%*%mu_t + sqrt(t(omega)%*%sigma%*%omega)*qt(p = alpha, df = 10)
      return(res)
    }
    
    eqn <- function(omega, sigma, alpha, mu_t) {
      return(c(sum(omega),t(omega)%*%mu_t))
    }
    
    #forecast H_t | t-1
    returns <- daily_return %>% 
      dplyr::filter(
        date < Date
      ) 
    returns_mat <- returns %>% 
      dplyr::select(-date) %>% 
      as.matrix()
    intraday <- intraday_refreshed %>% 
      dplyr::mutate(
        Start = rownames(.),
        date = lubridate::as_date(Start)
      ) %>% 
      dplyr::filter(
        date < Date
      )
    
    RC_list_filt <- RC_list[1:(which(names(RC_list) == as.character(Date))-1)]
    
    H_end <- get_H_t_all(RC_list = RC_list_filt, return_mat = returns_mat)$H_end
    res <- solnp(
      pars = as.matrix(
        rep(1/(ncol(daily_return)-1), ncol(daily_return)-1),
        ncol = 1
      ), 
      fun = VaR,
      sigma = H_end,
      alpha = 0.95,
      mu_t = mu_t,
      LB = rep(-2, ncol(intraday_refreshed)),#all unknowns are restricted to be positiv
      UB = rep(2, ncol(intraday_refreshed)),
      eqfun = eqn,
      eqB = c(1,0.08/250) #0.08/250
    )$pars
    
    return(res)
  }
  
  dates <- daily_return %>% 
    dplyr::filter(date >= start_date) %>% 
    .[["date"]]
  
  weights_list <- lapply(
    X = dates,
    FUN = get_weights,
    daily_return = daily_return,
    intraday_refreshed = intraday_refreshed,
    RC_list = RC_list,
    mu_t = mu_t
  )
  
  return(weights_list)
}

pnl_curves <- function(weights_list, daily_return, intraday_refreshed, start_date = "2023-10-02") {
  dates <- daily_return %>% 
    dplyr::filter(date >= start_date) %>% 
    .[["date"]]
  
  get_PnL <- function(weights_list, dates, daily_return, intraday_refreshed) {
    daily_pnl <- rep(0,length(dates))
    prices <- intraday_refreshed %>% 
      dplyr::mutate_if(is.numeric, exp)
    
    for (i in 1:length(dates)) {
      weights <- weights_list[[i]] %>% as.matrix()
      returns <- daily_return %>% 
        dplyr::filter(date == dates[[i]]) %>% 
        dplyr::select(-date) %>% 
        as.matrix()
      
      pricesB <- prices %>% 
        dplyr::mutate(
          Start = rownames(.), 
          date = lubridate::as_date(Start)
        ) %>% 
        dplyr::filter(date == dates[[i]]) %>% 
        tail(1) %>% 
        dplyr::select(-Start, -date) %>% 
        unname() %>% as.matrix()
      row.names(pricesB) <- NULL
      
      pricesA <- prices %>% 
        dplyr::mutate(
          Start = rownames(.), 
          date = lubridate::as_date(Start)
        ) %>% 
        dplyr::filter(date < dates[[i]]) %>% 
        tail(1) %>% 
        dplyr::select(-Start, -date) %>% 
        unname() %>% as.matrix()
      row.names(pricesA) <- NULL
      
      price_diff <- pricesB - pricesA
      
      daily_pnl[i] <- returns %*% weights
    }
    return(cumsum(daily_pnl))
  }
  
  buynhold <- function(dates, d = ncol(intraday_refreshed)) {
    all_weights <- lapply(
      X = 1:d,
      FUN = function(asset, dates, d) {
        lapply(
          X = dates,
          FUN = function(dates, asset, d) {
            a<- rep(0,d)
            a[asset] <- 1
            return(a)
          },
          asset = asset,
          d = d
        )
      },
      dates = dates,
      d = d
    )
  }
  all_weights <- buynhold(dates) 
  all_weights[[ncol(intraday_refreshed)+1]] <- weights_list
  all_weights <- all_weights %>% 
    magrittr::set_names(c(colnames(intraday_refreshed),"VaR"))
  
  pnl_curves <- lapply(
    all_weights,
    FUN = get_PnL,
    dates = dates, 
    daily_return = daily_return, 
    intraday_refreshed = intraday_refreshed
  )
  
  plot_list_of_curves_ggplot <- function(data_list) {
    # Combine the list of vectors into a data frame
    data_df <- data.frame(
      Index = rep(dates, length(data_list)),
      EUR = unlist(data_list)*100,
      Portfolio = rep(names(data_list), each = length(data_list[[1]]))
    )
    
    bar_df <- data_df %>% 
      dplyr::group_by(Portfolio) %>% 
      dplyr::summarise(EUR = tail(EUR,1))
    # Create the ggplot
    p1 <- ggplot(data_df, aes(x = Index, y = EUR, color = Portfolio)) +
      geom_line(size = 1) +
      labs(x = "Date", y = "Return (%)", title = "Cumulative PnL") +
      scale_color_viridis(discrete = TRUE, option = "D") +
      theme_minimal() + 
      theme(legend.position="none")
    
    p2 <- ggplot(bar_df, aes(x = Portfolio, y = EUR, fill = Portfolio)) + 
      geom_bar(stat="identity") +
      labs(x = "", y = "", title = "Final PnL") +
      scale_fill_viridis(discrete = TRUE, option = "D") +
      theme_minimal() +
      theme(axis.text.x=element_text(colour="white"))
    # theme(axis.text.x=element_blank())
    
    gridExtra::grid.arrange(p1,p2,ncol=2)
    # cowplot::plot_grid()
  }
  plot_list_of_curves_ggplot(pnl_curves)
  
  return(pnl_curves)
}

plot_weigth_curves <- function(weights_list, daily_return, intraday_refreshed, start_date = "2023-10-02") {
  dates <- daily_return %>% 
    dplyr::filter(date >= start_date) %>% 
    .[["date"]]
  
  weights_list <- weights_list %>% 
    unname()
  df <- data.frame(matrix(NA, nrow = length(dates), ncol = ncol(intraday_refreshed))) %>% 
    magrittr::set_colnames(c(colnames(intraday_refreshed)))
  
  for (i in 1:length(dates)) {
    df[i,] <- weights_list[[i]]*100
  }
  weights_df <- df %>% 
    dplyr::mutate(Date = dates) %>% 
    tidyr::gather("Asset", "Weight", -Date)
  
  box_weights_df <- weights_df %>% 
    dplyr::select(-Date)
  
  p1 <- ggplot(weights_df, aes(x = Date, y = Weight, color = Asset)) +
    geom_line(size = 1) +
    labs(x = "Date", y = "Weight (%)", title = "VaR Portfolio Weights") +
    scale_color_viridis(discrete = TRUE, option = "D") +
    theme_minimal() + 
    theme(legend.position="none")
  
  p2 <- ggplot(box_weights_df, aes(x=Asset, y=Weight, fill=Asset)) +
    geom_boxplot() +
    labs(x = "", y = "Weight (%)", title = "VaR Portfolio Weights") +
    theme_minimal() +
    theme(axis.text.x=element_text(colour="white")) +
    scale_fill_viridis(discrete = TRUE, option = "D") +
    geom_hline(yintercept=0, linetype='dashed', col = 'black')
  
  gridExtra::grid.arrange(p2,ncol=1)
}

### Data ----------------------------------------------------------------------
# load("daily_return_13.RData")
load("refresh_last.RData")

# daily_return <- tester

intraday_refreshed <- refresh_data %>% 
  dplyr::mutate_if(is.numeric, function(x){-x}) %>% 
  dplyr::select(-EURTRY)

daily_return <- data.frame(diff(as.matrix(intraday_refreshed))) %>% 
  dplyr::mutate(date = lubridate::date(rownames(.)), .before=1) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise_all(sum)

# return_mat <- daily_return %>% 
#   dplyr::select(-date) %>% 
#   as.matrix()
# 
# 
# H_end <- get_H_t_all(RC_list, return_mat)

