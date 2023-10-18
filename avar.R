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
  kn <- floor(theta*sqrt(nrow(Y)-1))
  return(kn)
}

Y_bar <- function(Y, kn, ...) {
  param <- unlist(list(...), recursive = F)
  
  fun_to_call <- 'g_fun'
  if ('a' %in% names(param)) {
    fun_to_call <- 'gi_fun'
  }
  
  dY <- diff(Y)
  g_vals <- sapply(X = 1:(kn-1)/kn, FUN = fun_to_call, param)
  
  n <- nrow(Y)-1
  Y_Res <- matrix(nrow=n-kn+2, ncol = ncol(Y))
  
  for (i in 0:(n-kn+1)) {
    res_to_append <- 0
    for (j in 1:(kn-1)){
      res_to_append <- res_to_append + g_vals[j]*dY[i+j,]
    }
    Y_Res[i+1, ] <- res_to_append
  }
  
  return(Y_Res)
}

chi_fun <- function(Ybari) {
  chi <- c(matrix(Ybari,ncol = 1) %*% matrix(Ybari,nrow = 1))
  return(chi)
}

V_statistic <- function(Ybar, kn, d = ncol(Y), n = nrow(Y)-1) {
  v1 <- matrix(rep(0, d^4), nrow = d^2, ncol=d^2)
  v2 <- matrix(rep(0, d^4), nrow = d^2, ncol=d^2)
  
  for (i in 0:(n - kn + 1)) {
    v1 <- v1 + chi_fun(Ybar[i+1,])%*%t(chi_fun(Ybar[i+1,]))
  }
  
  for (i in 0:(n - 2*kn + 1)){
    v2 <- v2 + chi_fun(Ybar[i+1,])%*%t(chi_fun(Ybar[i+kn+1,])) + 
      chi_fun(Ybar[i+kn+1,])%*%t(chi_fun(Ybar[i+1,])) 
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

phi1 <- function(j){
  v1 <- sapply(X=j:(kn-2)/kn, FUN = g_fun)
  v2 <- sapply(X=(j+1):(kn-1)/kn, FUN = g_fun)
  v3 <- sapply(X=0:(kn-j-2)/kn, FUN = g_fun)
  v4 <- sapply(X=1:(kn-j-1)/kn, FUN = g_fun)
  
  return(
    sum(
      (v1-v2)*(v3-v4)
    )
  )
}

phi2 <- function(j){
  v1 <- sapply(X=j:(kn-2)/kn, FUN = g_fun)
  v2 <- sapply(X=(j+1):(kn-1)/kn, FUN = g_fun)
  
  return(sum(v1-v2))
}

Phi11 <- function(kn) {
  v1 <- sapply(X=0:(kn-1), FUN = phi1)
  v2 <- phi1(0)
  
  return(kn*(sum(v1^2) - 0.5*v2^2))
}

Phi12 <- function(kn) {
  v1 <- sapply(X=0:(kn-1), FUN = phi1)*sapply(X=0:(kn-1), FUN = phi2)
  v2 <- phi1(0)*phi2(0)
  
  return((1/kn)*(sum(v1) - 0.5*v2))
}

Phi22 <- function(kn) {
  v1 <- sapply(X=0:(kn-1), FUN = phi2)
  v2 <- phi2(0)
  
  return((1/kn^3)*(sum(v1^2) - 0.5*v2^2))
}

invert_matrix3x3 <- function(a) {
  if (is.matrix(a) != T) stop("The input is not a matrix")
  if (ncol(a) != nrow(a)) stop("The matrix is not a square matrix")
  if (ncol(a)!=3 | nrow(a)!=3) stop("The matrix is not a 3x3 matrix")
  b <- matrix(rep(0,9), ncol=3)
  det1a <- (
    a[1,3]*a[2,2]*a[3,1] -
      a[1,2]*a[2,3]*a[3,1] -
      a[1,3]*a[2,1]*a[3,2] +
      a[1,1]*a[2,3]*a[3,2] +
      a[1,2]*a[2,1]*a[3,3] -
      a[1,1]*a[2,2]*a[3,3]
  )
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

A_matrix <- function(kn, theta, g_param1, g_param2, g_param3) {
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

C_vector <- function(A_inv, kn, theta) {
  v1 <- 2*Phi22(kn=kn)*theta/psi2(kn=kn)^2
  v2 <- 2*Phi12(kn=kn)/(psi2(kn=kn)^2*theta)
  v3 <- 2*Phi11(kn=kn)/(psi2(kn=kn)^2*theta^3)
  
  return(matrix(c(v1,v2,v3),nrow = 1)%*%A_inv)
}

avar_est <- function(Y) {
  g_param1 <- list('a' = 6, 'b' = 3)
  g_param2 <- list('a' = 4, 'b' = 5)
  g_param3 <- list('a' = 8, 'b' = 2)
  if (is.null(nrow(Y)) && length(Y)!=0) {
    Y <- matrix(Y, ncol = 1)
  }
  
  theta <- 0.8
  kn <<- kn_fun(Y,theta)
  #V(g1)
  Ybar_g1 <- Y_bar(Y, kn, g_param1)
  V1 <- V_statistic(Ybar_g1, kn)
  print("V1")
  
  #V(g2)
  Ybar_g2 <- Y_bar(Y, kn, g_param2)
  V2 <- V_statistic(Ybar_g2, kn)
  
  print("V2")
  #V(g3)
  Ybar_g3 <- Y_bar(Y, kn, g_param3)
  V3 <- V_statistic(Ybar_g3, kn)
  
  print("V3")
  #A matrix inverse
  A_mat <- A_matrix(kn, theta, g_param1, g_param2, g_param3)
  A_inv <- invert_matrix3x3(A_mat)
  
  print("A")
  #C vector
  C <- C_vector(A_inv, kn, theta)
  print("C")
  #avar
  avar <- C[1]*V1 + C[2]*V2 + C[3]*V3
  
  return(avar)
}

MRC_est <- function(Y){
  if (is.null(nrow(Y)) && length(Y)!=0) {
    Y <- matrix(Y, ncol = 1)
  }
  
  theta <- 0.8
  kn <- kn_fun(Y,theta)
  n <- nrow(Y) - 1
  d <- ncol(Y)
  
  Ybar <- Y_bar(Y,kn)
  v1 <- matrix(rep(0,d^2), ncol = d)
  psi_1 <- psi1(kn)
  psi_2 <- psi2(kn)
  
  for (i in 0:(n - kn + 1)) {
    v1 <- v1 + Ybar[i+1,]%*%t(Ybar[i+1,])
  }
  
  mrc <- n/(n-kn+2)*1/(psi_2*kn)*v1
  
  dY <- diff(Y)
  
  v2 <- matrix(rep(0, d^2), ncol = d)
  for (i in 1:nrow(dY)) {
    v2 <- v2 + dY[i,]%*%t(dY[i,])
  }
  
  ms_var <- 1/(2*n)*v2
  bias_correction <- psi_1/(theta^2*psi_2)*ms_var
  
  return(mrc - bias_correction)
}

confidence_intervals <- function(Y, mrc, avar) {
  N <- nrow(Y)-1
  d <- ncol(Y)
  
  results <- list()
  
  for (i in 1:d) {
    for (j in 1:i) {
      results[[paste(i,j,sep = ",")]] <- 
        list(
          "lower" = mrc[[i,j]] - 1.96*avar[[(i-1)*d+j,(i-1)*d+j]]/(N^(1/4)),
          "upper" = mrc[[i,j]] + 1.96*avar[[(i-1)*d+j,(i-1)*d+j]]/(N^(1/4))
        )
    }
  }
  
  return(results)
}

MAE <- function(true_cov, est) {
  return(mean(abs(est - true_cov)))
} 

RMSE <- function(true_cov, est) {
  return(sqrt(mean((est-true_cov)^2)))
}

### Testing -------------------------------------------------------------------
source("Euler_scheme.R")
sim <- simulate_prices(n_prices = 2, Tend = 1, N = 86400, gamma2 = 0.01)
sim$cov #Analytical cov
Y <- sim$XwN
mrc <- MRC_est(Y);mrc
avar <- avar_est(Y);avar
conf_int <- confidence_intervals(Y, mrc, avar);conf_int
MAE(sim$cov, mrc)
RMSE(sim$cov, mrc)



