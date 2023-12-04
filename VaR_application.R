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
})

source("helpfuns.R")
source("get_RC.R")
### Functions -----------------------------------------------------------------
portfolio_strategy <- function(
    daily_return,
    refresh_data, 
    start_date = "2023-10-02"
) {
  browser()
  RC_list <- get_daily_RC(refresh_data)
  get_weights <- function(Date, daily_return, refresh_data, RC_list) {
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
    intraday <- refresh_data %>% 
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
  
  get_PnL <- function(weights_list, dates, daily_return, refresh_data) {
    daily_pnl <- rep(0,length(dates))
    
    prices <- refresh_data %>% 
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
    return(sum(daily_pnl))
  }
  
  dates <- daily_return %>% 
    dplyr::filter(date >= start_date) %>% 
    .[["date"]]
  
  weights_list <- lapply(
    X = dates,
    FUN = get_weights,
    daily_return = daily_return,
    refresh_data = refresh_data,
    RC_list = RC_list
  )
  
  buynhold_weights <- lapply(
    X = dates,
    FUN = function(dates) {
      c(1,rep(0,12))
    }
  )
  
  get_PnL(weights_list = buynhold_weights, dates = dates, daily_return, refresh_data)
  
  browser()
  daily_return
  
}



### Data ----------------------------------------------------------------------
load("daily_return_13.RData")
load("refresh_last.RData")

daily_return <- tester

intraday_refreshed <- refresh_data

train_daily_return <- daily_return %>% 
  dplyr::filter(date < lubridate::as_date("2023-10-01"))

test_daily_return <- daily_return %>% 
  dplyr::filter(date >= lubridate::as_date("2023-10-01"))

train_intraday <- daily_return %>% 
  dplyr::filter(date < lubridate::as_date("2023-10-01"))

test_intraday <- daily_return %>% 
  dplyr::filter(date >= lubridate::as_date("2023-10-01"))

return_mat <- daily_return %>% 
  dplyr::select(-date) %>% 
  as.matrix()
H_end <- get_H_t_all(RC_list, return_mat)

