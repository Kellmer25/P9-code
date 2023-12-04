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

source("helpfuns.R")
source("get_RC.R")
### Functions -----------------------------------------------------------------
portfolio_strategy <- function(
    daily_return,
    intraday_refreshed, 
    start_date = "2023-10-02"
) {
  browser()
  RC_list <- get_daily_RC(intraday_refreshed)
  get_weights <- function(Date, daily_return, intraday_refreshed, RC_list) {
    VaR <- function(omega, sigma, alpha) {
      res <- sqrt(t(omega)%*%sigma%*%omega)*qnorm(alpha)
      return(res)
    }
    
    eqn <- function(omega, sigma, alpha) {
      return(sum(omega))
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
      LB = rep(-100, 13),#all unknowns are restricted to be positiv
      UB = rep(100, 13),
      eqfun = eqn,
      eqB = 1
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
    RC_list = RC_list
  )
  
  DCC_pnl <- get_PnL(weights_list = weights_list, dates = dates, daily_return, intraday_refreshed = intraday_refreshed)
  
  buynhold_pnl <- get_PnL(weights_list = buynhold_weights, dates = dates, daily_return, intraday_refreshed = intraday_refreshed)
  
  browser()
  daily_return
  
}

pnl_curves <- function(weights_list, daily_return, intraday_refreshed) {
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
  
  buynhold <- function(dates) {
    all_weights <- lapply(
      X = 1:13,
      FUN = function(asset, dates) {
        lapply(
          X = dates,
          FUN = function(dates, asset) {
            a<- rep(0,13)
            a[asset] <- 1
            return(a)
          },
          asset = asset
        )
      },
      dates = dates
    )
  }
  all_weights <- buynhold(dates) 
  all_weights[[14]] <- weights_list
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
}

plot_weigth_curves <- function(weights_list, daily_return, intraday_refreshed) {
  dates <- daily_return %>% 
    dplyr::filter(date >= start_date) %>% 
    .[["date"]]
  
  weights_list <- weights_list %>% 
    unname()
  
  df <- data.frame(matrix(NA, nrow = 40, ncol = 13)) %>% 
    magrittr::set_colnames(c(colnames(intraday_refreshed)))
  
  for (i in 1:40) {
    df[i,] <- weights_list[[i]]*100
  }
  weights_df <- df %>% 
    dplyr::mutate(Date = dates) %>% 
    tidyr::gather("Asset", "Weight", -Date)
  
  box_weights_df <- weights_df %>% 
    dplyr::select(-Date)
  dplyr::group_by(Asset) %>% 
    dplyr::summarise(Weight = mean(Weight))
  
  p1 <- ggplot(weights_df, aes(x = Date, y = Weight, color = Asset)) +
    geom_line(size = 1) +
    labs(x = "Date", y = "Weight (%)", title = "VaR Portfolio Weights") +
    scale_color_viridis(discrete = TRUE, option = "D") +
    theme_minimal() + 
    theme(legend.position="none")
  
  # p2 <- ggplot(bar_weights_df, aes(x = Asset, y = Weight, fill = Asset)) + 
  #   geom_bar(stat="identity") +
  #   labs(x = "", y = "", title = "Mean Weight") +
  #   scale_fill_viridis(discrete = TRUE, option = "D") +
  #   theme_minimal() +
  #   theme(axis.text.x=element_text(colour="white"))
  
  p2 <- ggplot(box_weights_df, aes(x=Asset, y=Weight, fill=Asset)) +
    geom_boxplot() +
    labs(x = "", y = "", title = "") +
    theme_minimal() +
    theme(axis.text.x=element_text(colour="white")) +
    scale_fill_viridis(discrete = TRUE, option = "D");p2
  
  gridExtra::grid.arrange(p1,p2,ncol=2)
  
}

### Data ----------------------------------------------------------------------
load("daily_return_13.RData")
load("refresh_last.RData")

# daily_return <- tester

intraday_refreshed <- refresh_data %>% 
  dplyr::mutate_if(is.numeric, function(x){-x})

daily_return <- data.frame(diff(as.matrix(intraday_refreshed))) %>% 
  dplyr::mutate(date = lubridate::date(rownames(.)), .before=1) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise_all(sum)

# train_daily_return <- daily_return %>% 
#   dplyr::filter(date < lubridate::as_date("2023-10-01"))
# 
# test_daily_return <- daily_return %>% 
#   dplyr::filter(date >= lubridate::as_date("2023-10-01"))
# 
# train_intraday <- daily_return %>% 
#   dplyr::filter(date < lubridate::as_date("2023-10-01"))
# 
# test_intraday <- daily_return %>% 
#   dplyr::filter(date >= lubridate::as_date("2023-10-01"))

return_mat <- daily_return %>% 
  dplyr::select(-date) %>% 
  as.matrix()
H_end <- get_H_t_all(RC_list, return_mat)

