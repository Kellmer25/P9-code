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
g_fun <- function(x,param = list()) {
  min(x,1-x)
}

gi_fun <- function(x,param) {
  a <- param$a
  b <- param$b
  
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
  chi <- matrix(nrow = nrow(Y), ncol = ncol(Y)^2)
  for (i in 1:nrow(Y)){
    chi[i,] <- c(Y[i,]%*%t(Y[i,]))
  }
  
  return(chi)
}

V_statistic <- function(chi, Y) {
  kn <- kn_fun(Y)  
  
  v1 <- matrix(rep(0, ncol(Y)^4), nrow = ncol(Y)^2, ncol=ncol(Y)^2)
  for (i in 1:nrow(chi)){
    v1 <- v1 + chi[i,]%*%t(chi[i,]) 
  }
  
  v2 <- matrix(rep(0, ncol(Y)^4), nrow = ncol(Y)^2, ncol=ncol(Y)^2)
  for (i in 1:(nrow(Y) - 2*kn + 2)){
    v2 <- v2 + chi[i,]%*%t(chi[i+kn,]) + chi[i+kn,]%*%t(chi[i,]) 
  }
  
  return(v1 - 0.5*v2)
}

psi1 <- function(kn, ...) {
  param <- unlist(list(...), recursive = F)
  
  fun_to_call <- 'g_fun'
  if ('a' %in% names(param)) {
    fun_to_call <- 'gi_fun'
  }
  
  v1 <- sapply(X = (1:kn)/kn,FUN = fun_to_call, param)
  v2 <- sapply(X = (0:(kn-1))/kn,FUN = fun_to_call, param)
  return(kn*sum((v1-v2)^2))
}

psi2 <- function(kn, ...) {
  param <- unlist(list(...), recursive = F)
  
  fun_to_call <- 'g_fun'
  if ('a' %in% names(param)) {
    fun_to_call <- 'gi_fun'
  }
  
  v1 <- sapply(X = (1:(kn-1))/kn,FUN = fun_to_call, param)
  return((1/kn)*sum(v1^2))
}

invert_matrix3x3 <- function(a) {
  if (is.matrix(a) != T) stop("The input is not a matrix")
  if (ncol(a) != nrow(a)) stop("The matrix is not a square matrix")
  if (ncol(a)!=3 | nrow(a)!=3) stop("The matrix is not a 3x3 matrix")
  b <- matrix(rep(0,9), ncol=3)
  det1a <- (a[1,3]*a[2,2]*a[3,1] - a[1,2]*a[2,3]*a[3,1] - a[1,3]*a[2,1]*a[3,2] + a[1,1]*a[2,3]*a[3,2] + a[1,2]*a[2,1]*a[3,3] - a[1,1]*a[2,2]*a[3,3])
  det2a <- -det1a
  if (det1a == 0) stop("The matrix is singular and not invertible")
  numb11 <- (a[2,3]*a[3,2] - a[2,2]*a[3,3])
  numb21 <- (a[2,1]*a[3,3] - a[2,3]*a[3,1])
  numb31 <- (a[2,1]*a[3,2] - a[2,2]*a[3,1])
  b[1,1] <- numb11 / det1a
  b[2,1] <- numb21 / det1a
  b[3,1] <- numb31 / det2a
  numb12 <- (a[1,3]*a[3,2] - a[1,2]*a[3,3])
  numb22 <- (a[1,3]*a[3,1] - a[1,1]*a[3,3])
  numb32 <- (a[1,1]*a[3,2] - a[1,2]*a[3,1])
  b[1,2] <- numb12 / det2a
  b[2,2] <- numb22 / det1a
  b[3,2] <- numb32 / det1a
  numb13 <- (a[1,3]*a[2,2] - a[1,2]*a[2,3])
  numb23 <- (a[1,1]*a[2,3] - a[1,3]*a[2,1])
  numb33 <- (a[1,1]*a[2,2] - a[1,2]*a[2,1])
  b[1,3] <- numb13 / det1a
  b[2,3] <- numb23 / det1a
  b[3,3] <- numb33 / det2a
  return(b)
}

A_matrix <- function(kn, theta) {
  g_param1 <- list('a' = 0.1, 'b' = 0.5)
  g_param2 <- list('a' = 0.2, 'b' = 0.4)
  g_param3 <- list('a' = 0.7, 'b' = 0.6)
  
  a_B <- function(kn, param, theta){
    return(theta^2*psi2(kn, param)^2)
  }
  a_M <- function(kn, param, theta){
    return(psi1(kn,param)*psi2(kn, param))
  }
  a_N <- function(kn, param, theta){
    return(psi1(kn, param)^2/theta^2)
  }
  
  A <- c(
    a_B(kn,g_param1,theta), a_M(kn,g_param1,theta), a_N(kn,g_param1,theta),
    a_B(kn,g_param2,theta), a_M(kn,g_param2,theta), a_N(kn,g_param2,theta),
    a_B(kn,g_param3,theta), a_M(kn,g_param3,theta), a_N(kn,g_param3,theta)
  ) %>% matrix(nrow = 3, ncol = 3, byrow = T)
  
  return(A)
}

### Testing -------------------------------------------------------------------
Y <- test$XwN[,c(1,2)]
Ybar <- Y_bar(Y)

chi <- chi_fun(Ybar)
V <- V_statistic(chi, Y)
list('x' = 0.5,'a' = 0.3, 'b' = 0.7) %>% 
  do.call(gi_fun, .)
