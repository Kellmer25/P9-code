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

### Functions -----------------------------------------------------------------
VaR <- function(omega) {
  sqrt(t(omega)%*%sigma%*%omega)*qnorm(alpha)
}

eqn <- function(omega) {
  return(sum(omega))
}

res <- solnp(
  pars = as.matrix(rep(1/13, 13),ncol = 1), 
  fun = VaR,
  eqfun = eqn,
  eqB = 1
)
