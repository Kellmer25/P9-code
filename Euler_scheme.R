### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(lubridate)
  library(magrittr)
  library(tidyr)
  library(tictoc)
  library(data.table)
  library(glue)
})

### Magic numbers -------------------------------------------------------------
Tend <- 6.5
N <- Tend*3600
beta0 <- -5/16
beta1 <- 1/8
Tinterval <- seq(0,Tend, by = Tend/N)

### Functions -----------------------------------------------------------------
OU_exact <- function(
    BM, alpha = -1/40, sigma = 1, Tend, N
){
  X0 <- rnorm(1,0,sqrt((-2*alpha)^(-1)))
  # X0 <- 22.1559
  OU <- rep(X0,N+1)
  t_inc <- Tend/N
  xi <- diff(BM)/sqrt(t_inc)
  
  for (k in 1:N) {
    OU[k+1] <- exp(alpha*t_inc)*OU[k] + 
      sigma*sqrt(-1/(2*alpha)*(1-exp(2*alpha*t_inc)))*xi[k] 
  }
  
  return(OU)
}

BM_exact <- function(Tend, N, ncols){
  BM <- matrix(nrow=N+1, ncol = ncols)
  t_inc <- Tend/N
  for (i in 1:ncols){
    BM[,i] <- cumsum(c(0,rnorm(N,0,sqrt(t_inc))))
  }
  
  return(BM)
}

EM_scheme <- function(BM, sigma, Tend, N, a = 0.03, rho = -0.3){
  a_func <- function(a,t_inc){
    return(a*t_inc)
  }
  
  b_func <- function(rho, sigma, BM_inc){
    return(rho*sigma*BM_inc[1] + sqrt(1-rho^2)*sigma*BM_inc[2])
  }
  
  X0 <- 4
  X <- rep(X0,N+1)
  t_inc <- Tend/N
  
  #Brownian Motions
  n_BM <- ncol(BM)
  BM_inc <- matrix(nrow = N,ncol = n_BM)
  
  for (i in 1:n_BM){
    BM_inc[,i] <- diff(BM[,i])  
  }
  
  for (i in 1:N){
    X[i+1] = X[i] + a_func(a,t_inc) + b_func(rho, sigma[i+1], BM_inc[i,])
  }
  
  return(X)
}

msNoise <- function(gamma2, sigma, X){
  N <- length(X)-1
  omega2 <- gamma2*sqrt(mean(sigma^2))
  
  noise <- rnorm(N+1, 0,sqrt(omega2))
  
  return(X+noise)
}

matrix_to_dt <- function(mat, return_list = F){
  # Define the start and end datetimes
  start_datetime <- as.POSIXct("2023-10-09 00:00:00", tz = "UTC")
  end_datetime <- as.POSIXct("2023-10-09 23:59:59", tz = "UTC")
  
  # Create the vector sequence
  DT_seq <- seq(start_datetime, end_datetime, by = "1 sec")
  
  if (return_list) {
    res_list <- list()
    for (i in 1:ncol(mat)){
      listToAppend <- list(
        data.table::setDT(data.frame("DT" = DT_seq, "Price" = mat[-1,i]))
      )
      res_list <- append(res_list, listToAppend)
    }
    
    return(res_list)
  }
  
  return(
    data.table::setDT(data.frame("DT" = DT_seq, "Price" = mat[-1,]))
  )
}

riemann_approx <- function(fun, Tend, N) {
  #Riemann approx using midpoint rule w/ linear interpolation
  t_inc <- Tend/N
  #linear interp
  vals <- (fun[-length(fun)] + fun[-1])/2
  return(sum(t_inc*vals))
}

approx_analytic_cov <- function(sigma, rho, Tend, N){
  n_prices <- ncol(sigma)
  res <- matrix(nrow = n_prices, ncol = n_prices)
  
  for (i in 1:n_prices) {
    for (j in 1:n_prices) {
      if (i == j){
        res[i,i] <- riemann_approx(sigma[,i]^2, Tend, N)
      } else {
        res[i,j] <- riemann_approx(
          sigma[,i]*sigma[,j]*sqrt(1-rho[i]^2)*sqrt(1-rho[j]^2), 
          Tend, N
        )
      }
      
    }
  }
  return(res)
}

simulate_prices <- function(n_prices, Tend, N, gamma2, return_cov = T) {
  BM <- BM_exact(Tend, N, n_prices+1)
  
  sigma <- matrix(nrow = N+1, ncol = n_prices)
  X <- matrix(nrow = N+1, ncol = n_prices)
  XwN <- matrix(nrow = N+1, ncol = n_prices)
  
  for (i in 1:n_prices){
    sigma[,i] <- exp(beta0 + beta1*OU_exact(BM[,i], Tend = Tend, N = N))
    X[,i] <- EM_scheme(BM = BM[,c(i,n_prices+1)], sigma[,i], Tend, N)
    XwN[,i] <- msNoise(gamma2, sigma[,i], X[,i])
  }
  
  if (return_cov) {
    cov <- approx_analytic_cov(
      sigma, rho = rep(x = 0.3,n_prices), Tend = Tend, N = N
    )
    return(list("XwN" = XwN, "cov" = cov))
  }
  
  return(XwN)
}

thinner_func <- function(matrix, freq) {
  end <- floor(nrow(matrix)/freq)
  indToKeep <- seq(1, end*freq, by=freq)
  
  return(matrix[indToKeep,])
}

sum_stats <- function(estimat, true_val) {
  bias <- sum(true_val - estimat)
  mae <- mean(abs(true_val - estimat))
  rmse <- sqrt(mean((true_val - estimat)^2))
  return(list("bias" = bias, "mae" = mae, "rmse" = rmse))
}

plot_proc <- function(x1,x2) {
  yMax <- max(c(x1, x2))
  yMin <- min(c(x1, x2))
  plot(Tinterval, x1, type = "l", ylim = c(yMin,yMax))
  lines(x=Tinterval, y=x2)
}

# Observation times
obs_times = function(N=23400, lambdas=c(3,6), df=FALSE) {
  len = length(lambdas)
  obs = list()
  for (i in 1:len) {
    inter = rexp(N, 1/lambdas[i])
    times = cumsum(inter)
    times = times[!times > 23400]
    obs[[i]] = times
  }
  if (isTRUE(df)) {
    max_col = max(sapply(obs, function(x) length(x)))
    for (i in 1:len) {
      nas = rep(NA, max_col - length(obs[[i]]))
      obs[[i]] = c(obs[[i]], nas)
    }
    cols = c()
    for (i in 1:len) {
      cols[i] = glue("lambda{lambdas[i]}")
    }
    obs = as.data.frame(obs) %>% 
      magrittr::set_colnames(cols)
  }
  return(obs)
}

### Test ----------------------------------------------------------------------
BM <- BM_exact(Tend, N, 6)

#OU Volatility
sigma1 <- exp(beta0 + beta1*OU_exact(BM[,-c(1,3)]))
sigma2 <- exp(beta0 + beta1*OU_exact(BM[,-c(2,3)]))

X1 <- EM_scheme(BM = BM[,-1], sigma1)
X2 <- EM_scheme(BM = BM[,-2], sigma2)

XwN1 <- msNoise(0.001, sigma1, X1)
XwN2 <- msNoise(0.001, sigma2, X2)

test <- simulate_prices(n_prices = 5, Tend = 24, N = 86400, gamma2 = 0.01)
test$cov

test_dt <- matrix_to_dt(exp(test$XwN), return_list = T)
mrc_est <- highfrequency::rMRCov(test_dt);mrc_est
sum_stats(mrc_est, test$cov)

test_dt_cov <- matrix_to_dt(exp(test$XwN), return_list = F)
cov_est <- highfrequency::rCov(thinner_func(test_dt_cov,1),makeReturns = T)
sum_stats(cov_est, test$cov)
### Plot ----------------------------------------------------------------------
par(mar = c(4,4,2,4), mfrow = c(2,1))
plot_proc(X1,X2)
plot_proc(XwN1, XwN2)
max(c(XwN1-X1,XwN2-X2))