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
  library(metaSEM)
})
# 
# return_mat <- readRDS("refreshed_forex")
# RC_list <- readRDS("RC_series")
### Functions -----------------------------------------------------------------
h_t <- function(theta, v_vec, return_mat){ # computing the conditional variance vector and collect into a matrix
  d <- ncol(return_mat)
  
  omega <- theta[1:d]
  A <-  theta[(d+1):(2*d)]
  B <- theta[(2*d + 1):(3*d)]
  
  h <- matrix(nrow = d, ncol = ncol(v_vec))
  h0 <- v_vec[,1] # initial value MRC from day 1
  h[,1] <- h0
  
  for (i in 1:(ncol(v_vec)-1)) {
    h[,i+1] <- omega + A*v_vec[,i] + B*h[,i]
  }
  return(h)
}

get_real_corr <- function(real_cov_list, v_vec, t){ 
  diag(v_vec[,t]^(-0.5)) %*% real_cov_list[[t]] %*% diag(v_vec[,t]^(-0.5))
}

get_P_bar <- function(RL_t) {
  P <- RL_t[[1]]
  
  for (i in 2:length(RL_t)) {
    P <- P + RL_t[[i]]
  }
  return(P/length(RL_t))
}

R_t <- function(theta, real_corr_mat_list, R_bar, P_bar){ # computing the conditional variance vector and collect into a matrix
  
  alpha <- theta[1]
  beta <- theta[2]
  
  R <- vector(mode = "list", length = length(real_corr_mat_list))
  R0 <- real_corr_mat_list[[1]] # initial value RL from day 1
  R[[1]] <- R0
  
  R_tilde <- (1 - beta) * R_bar - alpha * P_bar
  # R_tilde_ev <- eigen(R_tilde)$values
  # 
  # R_tilde <- (R_tilde + min(R_tilde_ev)* diag(rep(1,nrow(R_tilde)))) / 
  #   (1+min(R_tilde_ev))
  # 
  for (i in 1:(length(real_corr_mat_list)-1)) {
    R[[i+1]] <-  R_tilde + alpha * real_corr_mat_list[[i]] + beta * R[[i]]
  }
  return(R)
}

llik_h <- function(theta, v_vec, return_mat){
  d <- ncol(return_mat)
  
  omega <- theta[1:d]
  A <- theta[d:(2*d)]
  B <- theta[(2*d+1):(3*d)]
  
  l <- rep(0,ncol(v_vec)-1)
  h <- h_t(theta = theta, v_vec = v_vec, return_mat = return_mat)
  
  for (i in 1:(ncol(v_vec)-1)) {
    u <- (h[,i+1]^(-1/2))*return_mat[i,] %>% as.matrix() %>% unname() %>% t()
    l[i] <-  2*log(prod(h[,i+1]^(1/2))) + t(u)%*%u
  }
  return(0.5 * sum(l))
}

llik_r <- function(
    theta, real_corr_mat_list, R_bar, P_bar, theta_H1, v_vec, return_mat
){
  alpha <- theta[1]
  beta <- theta[2]
  
  l <- rep(0,length(real_corr_mat_list)-1)
  if (!metaSEM::is.pd((1-beta)*R_bar - alpha*P_bar)) {
    return(10000000000000)
  }
  R <- R_t(c(alpha,beta), real_corr_mat_list, R_bar, P_bar)
  R_inv <- purrr::map(.x = R, solve) 
  h <- h_t(theta = theta_H1, v_vec = v_vec, return_mat = return_mat)
  for (i in 1:(length(real_corr_mat_list)-1)) {
    u <- (h[,i+1]^(-1/2))*return_mat[i,] %>% as.matrix() %>% unname()
    l[i] <- log(det(R[[i+1]])) + 
      t(u) %*% R_inv[[i+1]] %*% u
  }
  
  return(0.5 * sum(l))
}

QLH <- function(RC_list, return_mat) {
  thetah <- rep(0.5, 3*ncol(return_mat))
  thetar <- c(0.3419889856, 0.0000004822)
  v_vec <- purrr::map_dfc(.x = RC_list, .f = diag) %>% 
    as.matrix() # each column is diagonal elements of an RC
  
  RL_t <- purrr::map(
    .x = 1:length(RC_list),
    .f = get_real_corr,
    real_cov_list = RC_list,
    v_vec = v_vec
  )
  
  P_bar <- get_P_bar(RL_t)
  
  # R_bar <- P_bar
  u_t <- return_mat * diag(cov(return_mat))
  R_bar <- cor(u_t)
  tic()
  theta_H1 <- solnp(
    pars = thetah, 
    fun = llik_h, 
    v_vec = v_vec, 
    return_mat = return_mat,
    LB = c(rep(0,length(thetah))),#all unknowns are restricted to be positiv
    UB = c(
      rep(Inf, 2*length(thetah)/3), 
      rep(1, length(thetah)/3) # diagonal elements of B are smaller than the unity
    )
  )$pars
  toc()
  
  tic()
  # Generate thetar candidates
  # thetar_list <- list()
  # while (length(thetar_list) < 100) {
  #   alpha <- 0.5
  #   beta <- 0.5
  # 
  #   while (!metaSEM::is.pd((1-beta)*R_bar - alpha*P_bar)) {
  #     beta <- runif(1,0,1)
  #     alpha <- runif(1,0,1-beta)
  #   }
  #   thetar_list[[length(thetar_list)+1]] <- c(alpha,beta)
  # }
  # browser()
  # res <- lapply(
  #   thetar_list, solnp, fun = llik_r,
  #   real_corr_mat_list = RL_t,
  #   R_bar = R_bar,
  #   P_bar = P_bar,
  #   theta_H1 = theta_H1,
  #   v_vec = v_vec,
  #   return_mat = return_mat,
  #   LB = c(rep(0,length(thetar))), # all unknowns are restricted to be positive
  #   UB = c(Inf, 1)
  # )
  # final_param_index <- 0
  # value <- 100000
  # for (i in 1: 100) {
  #   if (value>(res[[i]]$values %>% tail(1))){
  #     value <- (res[[i]]$values %>% tail(1))
  #     final_param_index <- i
  #   }
  # }
  # theta_H2 <- thetar_list[[final_param_index]]

  theta_H2 <- solnp(
    pars = thetar,
    fun = llik_r,
    real_corr_mat_list = RL_t,
    R_bar = R_bar,
    P_bar = P_bar,
    theta_H1 = theta_H1,
    v_vec = v_vec,
    return_mat = return_mat,
    ineqfun = function(
    pars,
    real_corr_mat_list,
    R_bar,
    P_bar,
    theta_H1,
    v_vec,
    return_mat
    ){return(1-pars[2]-pars[1])},
    ineqLB = 0,
    ineqUB = 1,
    LB = c(rep(0,length(thetar))), # all unknowns are restricted to be positive
    UB = c(Inf, 1)
  )$pars
  toc()
  
  return("theta_H" = c(theta_H1, theta_H2))
}

get_H_t = function(cond_var_vec, cond_corr_mat, t){ 
  diag(cond_var_vec[,t]^(1/2)) %*% 
    cond_corr_mat[[t]] %*% 
    diag(cond_var_vec[,t]^(1/2))
}

get_H_t_all <- function(RC_list, return_mat){
  
  theta_H <- QLH(RC_list = RC_list, return_mat = return_mat)

  v_vec <- purrr::map_dfc(.x = RC_list, .f = diag) %>% 
    as.matrix() # each column is diagonal elements of an RC
  RL_t <- purrr::map(
    .x = 1:length(RC_list),
    .f = get_real_corr,
    real_cov_list = RC_list,
    v_vec = v_vec
  )
  
  P_bar <- get_P_bar(RL_t)
  
  u_t <- return_mat * diag(cov(return_mat))
  R_bar <- cor(u_t)
  d <- ncol(return_mat)
  
  h <- h_t(
    theta = theta_H[1:(3 * d)],
    v_vec = v_vec,
    return_mat = return_mat
  )
  
  R <- R_t(
    theta = theta_H[(3 * d + 1):(3 * d + 2)],
    real_corr_mat_list = RL_t,
    R_bar,
    P_bar
  )
  H_end <- get_H_t(t = length(RC_list), cond_var_vec = h, cond_corr_mat = R)
  
  return(list("H_end" = H_end, "theta_H" = theta_H))
}

