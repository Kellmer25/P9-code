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
VaR <- function(omega, sigma, alpha) {
  res <- sqrt(t(omega)%*%sigma%*%omega)*qnorm(alpha)
  return(res)
}

eqn <- function(omega, sigma, alpha) {
  return(sum(omega))
}

res <- solnp(
  pars = as.matrix(rep(1/13, 13),ncol = 1), 
  fun = VaR,
  sigma = H_end,
  alpha = 0.95,
  LB = rep(-100, 13),#all unknowns are restricted to be positiv
  UB = rep(100, 13),
  eqfun = eqn,
  eqB = c(1,0)
)


