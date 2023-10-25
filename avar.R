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

V_statistic <- function(Ybar, kn, d = ncol(Y), n = nrow(Ybar)-1) {
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
  scaling_const <- 1/(1-psi_1/(theta^2*psi_2*2*n))
  
  mrc_result <- scaling_const*(mrc - bias_correction)
  
  
  return(mrc_result)
}

confidence_intervals <- function(Y, mrc, avar) {
  n <- nrow(Y)-1
  d <- ncol(Y)
  
  results <- list()
  upper_mat <- mrc + matrix(1.96*diag(avar)/(n^(1/4)), ncol = d)
  lower_mat <- mrc - matrix(1.96*diag(avar)/(n^(1/4)), ncol = d)
  
  return(list("lower" = lower_mat, "upper" = upper_mat))
}

RC_est <- function(Y) {
  if (is.null(nrow(Y)) && length(Y)!=0) {
    Y <- matrix(Y, ncol = 1)
  }
  
  n <- nrow(Y) - 1
  d <- ncol(Y)
  
  res <- matrix(rep(0,d^2), ncol = d)
  dY <- diff(Y)
  for (i in 1:n) {
    res <- res + dY[i,]%*%t(dY[i,])
  }
  return(res)
}

MAE <- function(true_cov, est) {
  return(mean(abs(est - true_cov)))
} 

RMSE <- function(true_cov, est) {
  return(sqrt(mean((est-true_cov)^2)))
}

rolling_window_estimation <- function(days, nprday, n_prices, gamma2) {
  simulation <- simulate_prices(
    n_prices = n_prices, 
    Tend = days, 
    N = days*nprday, 
    gamma2 = gamma2
  )
  
  Y <- simulation$XwN
  
  mrc_estimates <- list()
  rc_estimates <- list()
  avar_estimates <- list()
  mrc_conf <- list()
  true_cov <- list()
  
  for (i in 1:days) {
    from <- (i-1)*nprday+1
    to <- i*nprday+1
    Ydaily <- Y[from:to,]
    
    mrc_estimates[[paste0(i)]] <- MRC_est(Ydaily)
    rc_estimates[[paste0(i)]] <- RC_est(Ydaily)
    avar_estimates[[paste0(i)]] <- avar_est(Ydaily)
    mrc_conf[[paste0(i)]] <- confidence_intervals(
      Y = Ydaily,
      mrc = mrc_estimates[[paste0(i)]],
      avar = avar_estimates[[paste0(i)]]
    )
    true_cov[[paste0(i)]] <- simulation$cov[[paste0(i)]] 
  }
  
  return(list(
    "mrc_estimates" = mrc_estimates,
    "rc_estimates" = rc_estimates,
    "avar_estimates" = avar_estimates,
    "mrc_conf" = mrc_conf,
    "cov" = true_cov
  ))
}

confidence_plot <- function(rwest_result) {
  d <- ncol(rwest_result$mrc_estimates$`1`)
  
  mrcTS_11 <- rep(0,length(rwest_result$mrc_estimates))
  mrcTS_11_upper <- rep(0,length(rwest_result$mrc_estimates))
  mrcTS_11_lower <- rep(0,length(rwest_result$mrc_estimates))
  true_cov <- rep(0,length(rwest_result$mrc_estimates))
  
  for (i in 1:length(mrcTS_11)) {
    mrcTS_11[i] <- rwest_result$mrc_estimates[[i]][[1,1]]
    mrcTS_11_upper[i] <- rwest_result$mrc_conf[[i]]$upper[[1,1]]
    mrcTS_11_lower[i] <- rwest_result$mrc_conf[[i]]$lower[[1,1]]
    true_cov[i] <- rwest_result$cov[[i]][[1,1]]
  }
  res <- data.frame(
    "day" = 1:30, true_cov, mrcTS_11, mrcTS_11_lower, mrcTS_11_upper
  )
  
  p <- ggplot(res, aes(day)) +
    geom_ribbon(
      aes(ymin=mrcTS_11_lower,ymax=mrcTS_11_upper),
      fill = "grey70"
    ) +
    geom_line(aes(y = mrcTS_11)) +
    geom_line(aes(y = true_cov))
  
  return(p)
}

update_sampling_freq <- function(Y, lambdas) {
  d <- ncol(Y)
  n <- nrow(Y) - 1
  
  for (i in 1:d) {
    poisson_proc <- cumsum(rexp(2*n, 1/lambdas[[i]]))
    end_index <- head(which(poisson_proc>n),1)
    updated_poisson <- poisson_proc[1:end_index]
    sampling <- sapply(X = updated_poisson, FUN = ceiling) %>%
      unique()
    
    new_sampling <- rep(1,n+1)
    start_index <- 1
    for (j in 2:(n+1)) {
      if (is.na(sampling[start_index]+1)){browser()}
      # print(j);print(sampling[start_index]+1)
      if (j == sampling[start_index]+1) {
        new_sampling[j] <- sampling[start_index] + 1
        start_index <- start_index+1
      } else {
        new_sampling[j] <- new_sampling[j-1]
      }
    }
    Y[,i] <- Y[new_sampling,i]
  }
  return(Y)
}

simulation <- function(lambda1, lambda2){
  apply_noise <- function(EffPriceMat, sigma, gamma2) {
    res_mat <- EffPriceMat
    for (i in 1:ncol(EffPriceMat)){
      res_mat[,i] <- msNoise(gamma2, sigma[,i], X = EffPriceMat[,i])
    }
    return(res_mat)
  }
  
  #Generate effiecient price
  EfficientPrice <- simulate_prices(2, Tend = 1, N = 86400, gamma2 = 0)
  
  #Apply three levels of noise
  Y1 <- EfficientPrice$X
  Y2 <- apply_noise(EfficientPrice$X, EfficientPrice$sigma, gamma2 = 0.001)
  Y3 <- apply_noise(EfficientPrice$X, EfficientPrice$sigma, gamma2 = 0.01)
  
  #Update sampling frequency of the three prices
  Y1PriceAS <- update_sampling_freq(Y1,lambdas = list(lambda1, lambda2))
  Y2PriceAS <- update_sampling_freq(Y2,lambdas = list(lambda1, lambda2))
  Y3PriceAS <- update_sampling_freq(Y3,lambdas = list(lambda1, lambda2))
  
  #Define the true Cov
  TrueCov <- EfficientPrice[["cov"]][[1]]
  browser()
  #Microstructure noise / no asynchronization
  RC1 <- RC_est(Y1)
  RC2 <- RC_est(Y2)
  RC3 <- RC_est(Y3)
  
  MRC1 <- MRC_est(Y1)
  MRC2 <- MRC_est(Y2)
  MRC3 <- MRC_est(Y3)
  
  res1 <- data.frame(
    'MSLevel' = 1:3, 
    'RC_RMSE' = c(RMSE(TrueCov,RC1), RMSE(TrueCov,RC2), RMSE(TrueCov,RC3)),
    'RC_MAE'  = c(MAE(TrueCov,RC1), MAE(TrueCov,RC2), MAE(TrueCov,RC3)),
    'MRC_RMSE'= c(RMSE(TrueCov,MRC1), RMSE(TrueCov,MRC2), RMSE(TrueCov,MRC3)),
    'MRC_MAE' = c(MAE(TrueCov,MRC1), MAE(TrueCov,MRC2), MAE(TrueCov,MRC3))
    )
  
  #Microstructure noise / asynchronization
  RC1 <- RC_est(Y1PriceAS)
  RC2 <- RC_est(Y2PriceAS)
  RC3 <- RC_est(Y3PriceAS)
  
  MRC1 <- MRC_est(Y1PriceAS)
  MRC2 <- MRC_est(Y2PriceAS)
  MRC3 <- MRC_est(Y3PriceAS)
  
  res2 <- data.frame(
    'MSLevel' = 1:3, 
    'RC_RMSE' = c(RMSE(TrueCov,RC1), RMSE(TrueCov,RC2), RMSE(TrueCov,RC3)),
    'RC_MAE'  = c(MAE(TrueCov,RC1), MAE(TrueCov,RC2), MAE(TrueCov,RC3)),
    'MRC_RMSE'= c(RMSE(TrueCov,MRC1), RMSE(TrueCov,MRC2), RMSE(TrueCov,MRC3)),
    'MRC_MAE' = c(MAE(TrueCov,MRC1), MAE(TrueCov,MRC2), MAE(TrueCov,MRC3))
  )
  
  return(list("Res1" = res1, "Res2" = res2))
}

### Testing -------------------------------------------------------------------
source("Euler_scheme.R")
sim <- simulate_prices(n_prices = 2, Tend = 1, N = 86400, gamma2 = 0.001)
sim$cov #Analytical cov
Y <- sim$XwN
mrc <- MRC_est(Y);mrc
rc <- RC_est(Y);rc
avar <- avar_est(Y);avar
conf_int <- confidence_intervals(Y, mrc, avar);conf_int
#MRC
MAE(sim$cov, mrc);RMSE(sim$cov, mrc)
#RC
MAE(sim$cov, rc);RMSE(sim$cov, rc)

pdata <- matrix_to_dt(exp(Y), return_list = T)
hf_mrc <- highfrequency::rMRCov(pdata,crossAssetNoiseCorrection = T, makePsd = F, theta = 0.8);hf_mrc
hf_mrc - mrc

updated_Y <- update_sampling_freq(
  Y, 
  lambdas = list("lambda1" = 1, "lambda2" = 1)
)

set.seed(123)
rwest_result <- rolling_window_estimation(30, 10000, 2, 0.001)
saveRDS(rwest_result, "rwest_result")

TEST <- simulation(lambda1 = 1, lambda2 = 3)




