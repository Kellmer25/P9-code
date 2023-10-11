### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(lubridate)
  library(magrittr)
  library(tidyr)
  library(tictoc)
  library(data.table)
})

### functions -----------------------------------------------------------------
g_fun <- function(x){
  min(x,1-x)
}

gi_fun <- function(x,a,b) {
  x^a*(1-x)^b
}

kn_fun <- function(Y, theta = 0.8) {
  kn <- floor(theta*sqrt(nrow(Y)))
  return(kn)
}

# Y_diff <- function(Y,I){
#   return(Y[I+1] - Y[I])
# }

Y_bar <- function(Y) {
  kn <- kn_fun(Y)
  # browser()
  Y_Res <- matrix(nrow=nrow(Y)-kn+2, ncol = ncol(Y))
  
  for (i in 1:nrow(Y_Res)){
    # browser()
    y_1 <- colSums(Y[(i+kn/2):(i+kn-1),])
    y_2 <- colSums(Y[(i):(i+kn/2-1),])
    Y_Res[i,] <- 1/kn * (y_1 - y_2) 
  }
  
  return(Y_Res)
}

chi_fun <- function(Y) {
  chi <- vector("list", nrow(Y))
  for (i in 1:nrow(Y)){
    chi[i,] <- Y[i,]
  }
  Y[]
  return()
}


### Testing -------------------------------------------------------------------
Y <- test$XwN[,c(1,2)]
Ybar <- Y_bar(Y)

chi <- chi_fun(Ybar)
