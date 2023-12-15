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
  library(purrr)
})

source("helpfuns.R")
source("get_RC.R")
source("avar.R")
source("DCC-HEAVY_forecasting.R")
### Functions -----------------------------------------------------------------
portfolio_strategy <- function(
    daily_return,
    intraday_refreshed, 
    start_date = "2023-10-02",
    return_H = F,
    t = F,
    mu = 0
) {
  
  mu_t <- daily_return %>% 
    dplyr::filter(date < lubridate::as_date("2023-10-02")) %>% 
    dplyr::select(-date) %>% 
    as.matrix() %>% 
    colMeans()
  
  RC_list <- get_daily_RC(intraday_refreshed)
  
  get_weights <- function(Date, daily_return, intraday_refreshed, RC_list, mu_t, return_H = return_H, t = t, mu = mu) {
    VaR <- function(omega, sigma, alpha, mu_t, t = t) {
      if (t) {
        quantile <- qt(p = alpha, df=10)
      } else {
        quantile <- qnorm(p = alpha)
      }
      res <- -t(omega)%*%mu_t + sqrt(t(omega)%*%sigma%*%omega)*quantile
      return(res)
    }
    
    eqn <- function(omega, sigma, alpha, mu_t, t) {
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
    # intraday <- intraday_refreshed %>% 
    #   dplyr::mutate(
    #     Start = rownames(.),
    #     date = lubridate::as_date(Start)
    #   ) %>% 
    #   dplyr::filter(
    #     date < Date
    #   )
    RC_list_filt <- RC_list[1:(which(names(RC_list) == as.character(Date))-1)]
    
    H_end <- get_H_t_all(RC_list = RC_list_filt, return_mat = returns_mat)$H_end
    if (return_H) {
      return(H_end)
    }
    # res <- solnp(
    #   pars = as.matrix(
    #     rep(1/(ncol(daily_return)-1), ncol(daily_return)-1),
    #     ncol = 1
    #   ), 
    #   fun = VaR,
    #   sigma = H_end,
    #   alpha = 0.95,
    #   mu_t = mu_t,
    #   LB = rep(-2, ncol(intraday_refreshed)),#all unknowns are restricted to be positiv
    #   UB = rep(2, ncol(intraday_refreshed)),
    #   eqfun = eqn,
    #   eqB = c(1,0) #0.08/250
    # )$pars
    
    res <- solnp(
      pars = as.matrix(
        rep(1/(ncol(daily_return)-1), ncol(daily_return)-1),
        ncol = 1
      ), 
      fun = VaR,
      sigma = H_end,
      alpha = 0.95,
      mu_t = mu_t,
      t = t,
      LB = rep(-2, ncol(intraday_refreshed)),#all unknowns are restricted to be positiv
      UB = rep(2, ncol(intraday_refreshed)),
      eqfun = eqn,
      eqB = c(1,mu) #0.08/250
    )$values %>% tail(1)
    
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
    mu_t = mu_t,
    return_H = return_H,
    t = t,
    mu = mu
  )
  
  return(list(RC_list, weights_list))
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
    # dplyr::mutate(Portfolio = dplyr::if_else(Portfolio == "VaR", "aVaR", Portfolio))
    
    bar_df <- data_df %>% 
      dplyr::group_by(Portfolio) %>% 
      dplyr::summarise(EUR = tail(EUR,1))
    # Create the ggplot
    p1 <- ggplot(data_df, aes(x = Index, y = EUR, color = Portfolio, alpha = Portfolio)) +
      geom_line(data = data_df, size = 1) +
      labs(x = "Date", y = "Return (%)", title = "Cumulative PnL") +
      scale_color_manual(values = c(rev(viridis(13)))) +
      scale_alpha_manual(values = c(rep(0.4,12),1), guide = guide_legend(title = "Portfolio")) +
      # scale_color_viridis(discrete = TRUE, option = "D") +
      theme_minimal() + 
      theme(legend.position="none")
    
    p2 <- ggplot(bar_df, aes(x = Portfolio, y = EUR, fill = Portfolio, alpha = Portfolio)) + 
      geom_bar(stat="identity") +
      labs(x = "", y = "", title = "Final PnL") +
      scale_fill_manual(values = c(rev(viridis(13)))) +
      scale_alpha_manual(values = c(rep(0.5,12),1), guide = guide_legend(title = "Portfolio")) +
      # scale_fill_viridis(discrete = TRUE, option = "D") +
      theme_minimal() +
      theme(axis.text.x=element_text(colour="white")) 
    # scale_fill_discrete(labels=c('VaR', rep("test", 12)))
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
    geom_boxplot(alpha = 0.6) +
    labs(x = "", y = "Weight (%)", title = "VaR Portfolio Weights") +
    theme_minimal() +
    theme(axis.text.x=element_text(colour="white")) +
    scale_fill_viridis(discrete = TRUE, option = "D") +
    geom_hline(yintercept=0, linetype='dashed', col = 'black') +
    coord_cartesian(ylim = c(-30, 65))
  
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

RC_H_list <- portfolio_strategy(daily_return,intraday_refreshed,return_H = T)
RC_list <- RC_H_list[[1]][(length(RC_H_list[[1]])-39):length(RC_H_list[[1]])]
H_list <- RC_H_list[[2]]
weights_g <- readRDS("weights_g")

get_RC_prod <- function(i, RC, weights_g) {
  return(sqrt(t(weights_g[[i]])%*%RC[[i]]%*%weights_g[[i]]))
}
get_H_prod <- function(i, H, weights_g) {
  return(sqrt(t(weights_g[[i]])%*%H[[i]]%*%weights_g[[i]]))
}
cov_vec <- sapply(X = 1:40, FUN = get_RC_prod, RC = r_cov, weights_g = weights_g)
H_vec <- sapply(X = 1:40, FUN = get_H_prod, H = H_list, weights_g = weights_g)

daily_return_filt[i,2:13] %>% 
  as.matrix()

r_cov <- lapply(
  X = 1:40,
  FUN = function(i, daily_return_filt, return_means) {
    r <- daily_return_filt[i,2:13] %>% 
      as.matrix()
    return(t(r - return_means)%*%(r-return_means))
  },
  daily_return_filt = daily_return_filt,
  return_means = return_means
)

FC_perf <- data.frame(
  "Date" = rep(dates,2),
  "Variance" = c(cov_vec, H_vec),
  "Forecast" = c(rep("N", 40),rep("Y", 40))
)

ggplot2::ggplot(data = FC_perf, aes(x = Date, y = Variance, color = Forecast)) +
  geom_line()

# return_mat <- daily_return %>% 
#   dplyr::select(-date) %>% 
#   as.matrix()
# 
# 
# H_end <- get_H_t_all(RC_list, return_mat)

png("weights_t_mu.png", height = 550, width = 1000)
plot_weigth_curves(weights_list = weights_t_mu, daily_return,intraday_refreshed)
dev.off()

weights_g_mu <- readRDS("weights_g_mu")
weights_t <- readRDS("weights_t")
weights_t_mu <- readRDS("weights_t_mu")

png(filename = "weights_g.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_g, daily_return,intraday_refreshed);dev.off()

png(filename = "weights_g_mu.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_g_mu, daily_return,intraday_refreshed);dev.off()

png(filename = "weights_t.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_t, daily_return,intraday_refreshed);dev.off()

png(filename = "weights_t_mu.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_t_mu, daily_return,intraday_refreshed);dev.off()


vars <- portfolio_strategy(daily_return, intraday_refreshed, t = F, mu = 0)
vars_mu <- portfolio_strategy(daily_return, intraday_refreshed, t = F, mu = 0.08/250)
vars_t <- portfolio_strategy(daily_return, intraday_refreshed, t = T, mu = 0)
vars_t_mu <- portfolio_strategy(daily_return, intraday_refreshed, t = T, mu = 0.08/250)

vars_df <- data.frame(
  Vars = c(
    unlist(vars),
    unlist(vars_mu),
    unlist(vars_t),
    unlist(vars_t_mu)
  ),
  Mu = c(
    rep("No",length(vars)),
    rep("Yes",length(vars)),
    rep("No",length(vars)),
    rep("Yes",length(vars))
  ),
  Distribution = c(
    rep("Gaussian",length(vars)),
    rep("Gaussian",length(vars)),
    rep("Student's t",length(vars)),
    rep("Student's t",length(vars))
  )
)

p1 <- ggplot2::ggplot(
  data = vars_df %>% 
    dplyr::filter(Mu == "Yes"), 
  aes(x = Distribution, y = Vars, fill = Distribution)
) + ggplot2::geom_boxplot(alpha = 0.4) + 
  labs(x = expression(paste(mu,' = 0.0003')), y = "VaR", title = " Distribution of Forecasted VaR") +
  scale_fill_manual(values = c("#443a83", "#20a486")) +
  theme_minimal() +
  theme(legend.position = "none")
p2 <- ggplot2::ggplot(
  data = vars_df %>% 
    dplyr::filter(Mu == "No"), 
  aes(x = Distribution, y = Vars, fill = Distribution)
) + ggplot2::geom_boxplot(alpha = 0.4) +
  labs(x = expression(paste(mu,' = 0')), y = "VaR", title = "") +
  scale_fill_manual(values = c("#443a83", "#20a486")) +
  theme_minimal() +
  theme(legend.position = "none")

cowplot::plot_grid(p1, p2, ncol = 2)
