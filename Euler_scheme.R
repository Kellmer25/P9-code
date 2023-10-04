### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(lubridate)
  library(magrittr)
  library(tidyr)  
})

### Magic numbers -------------------------------------------------------------
Tend <- 6.5
N <- Tend*3600
beta0 <- -5/16
beta1 <- 1/8
Tinterval <- seq(0,Tend, by = Tend/N)

### Functions -----------------------------------------------------------------
OU_exact <- function(alpha = -1/40, sigma = 1, Tend = 6.5, N = 23400){
  X0 <- rnorm(1,0,sqrt((-2*alpha)^(-1)))
  # X0 <- 22.1559
  OU <- rep(X0,N+1)
  xi <- rnorm(N,0,1)
  t_inc <- Tend/N
  
  for (k in 1:N) {
    OU[k+1] <- exp(alpha*t_inc)*OU[k] + 
      sigma*sqrt(-1/(2*alpha)*(1-exp(2*alpha*t_inc)))*xi[k] 
  }
  return(OU)
}

BM_exact <- function(Tend, N, ncols){
  BM <- matrix(nrow=N+1, ncol = ncols)
  t_inc <- T/N
  for (i in 1:ncols){
    BM[,i] <- cumsum(c(0,rnorm(N,0,sqrt(t_inc))))
  }
  return(BM)
}

a_func <- function(a,t_inc){
  return(a*t_inc)
}

b_func <- function(rho, sigma, BM_inc){
  return(rho*sigma*BM_inc[1] + sqrt(1-rho^2)*sigma*BM_inc[2])
}

EM_scheme <- function(BM, sigma, Tend=6.5, N = 23400, a = 0.03, rho = -0.3){
  X0 <- 100
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

msNoise <- function(gamma, sigma, X, N){
  omega2 <- gamma^2*sqrt(sum(sigma^4))
  
  noise <- rnorm(N+1, 0,sqrt(omega2))
  return(X+noise)
}

plot_proc <- function(x1,x2) {
  yMax <- max(c(x1, x2))
  yMin <- min(c(x1, x2))
  plot(Tinterval, x1, type = "l", ylim = c(yMin,yMax))
  lines(x=Tinterval, y=x2)
}

### ---------------------------------------------------------------------------
BM <- BM_exact(Tend, N, 3)

#OU Volatility
sigma1 <- OU_exact()
sigma2 <- OU_exact()

X1 <- EM_scheme(BM = BM[,-1], sigma1)
X2 <- EM_scheme(BM = BM[,-2], sigma2)

XwN1 <- msNoise(0.002, sigma1, X1, N)
XwN2 <- msNoise(0.002, sigma2, X2, N)


### Plot ----------------------------------------------------------------------
par(mar = c(4,4,2,4), mfrow = c(2,1))
plot_proc(X1,X2)
plot_proc(XwN1, XwN2)
