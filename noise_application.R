### Dependencies
source("helpfuns.R")
source("VaR_application")


### Functions
msNoise <- function(gamma2, sigma, X){
  N <- length(X)-1
  omega2 <- gamma2*sqrt(mean(sigma^2))
  
  noise <- rnorm(N+1, 0,sqrt(omega2))
  
  return(X+noise)
}



### Magic numbers
gamma = 0.001


### Data
forex_data = get_forex_data()
forex_data["EURTRY"] = NULL
refreshed = refresh_list(forex_data)

### Spot Volatility
sigma = apply(refreshed, MARGIN=2, FUN = sd)


### Microstructure Noise

#Before
get_ms_noise(refreshed) %>% diag() %>% norm(type="2") %>% format(scientific = TRUE)

# Add Nois
noised = apply(refreshed, MARGIN=2, FUN=msNoise, gamma2=gamma, sigma=sigma) %>% 
  as.data.frame()

# After
get_ms_noise(noised) %>% diag() %>% norm(type="2") %>% format(scientific = TRUE)
  

### VaR
VaR_weights_g <- portfolio_strategy(daily_return,intraday_refreshed)
VaR_weights_g_mu <- portfolio_strategy(daily_return,intraday_refreshed,mu = 0.08/250)
VaR_weights_t <- portfolio_strategy(daily_return,intraday_refreshed, t = T)
VaR_weights_t_mu <- portfolio_strategy(daily_return,intraday_refreshed, t = T, mu = 0.08/250)

list_to_list <- function(list, index) {
  final_list <- vector(mode = "list")
  for (i in 1:40) {
    final_list[i] <- list[[i]][index]
  }
  return(final_list)
}

weights_g <- list_to_list(VaR_weights_g, 2)
weights_g_mu <- list_to_list(VaR_weights_g_mu, 2)
weights_t <- list_to_list(VaR_weights_t, 2)
weights_t_mu  <- list_to_list(VaR_weights_t_mu, 2)

vars <- list_to_list(VaR_weights_g, 1)
vars_mu <- list_to_list(VaR_weights_g_mu, 1)
vars_t <- list_to_list(VaR_weights_t, 1)
vars_t_mu <- list_to_list(VaR_weights_t_mu, 1)

png(filename = "pnl_g_noise.png", width = 1000, height = 450)
pnl_curves(weights_list = weights_g, daily_return,intraday_refreshed);dev.off()

png(filename = "pnl_g_mu_noise.png", width = 1000, height = 450)
pnl_curves(weights_list = weights_g_mu, daily_return,intraday_refreshed);dev.off()

png(filename = "pnl_t_noise.png", width = 1000, height = 450)
pnl_curves(weights_list = weights_t, daily_return,intraday_refreshed);dev.off()

png(filename = "pnl_t_mu_noise.png", width = 1000, height = 450)
pnl_curves(weights_list = weights_t_mu, daily_return,intraday_refreshed);dev.off()


png(filename = "weights_g_noise.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_g, daily_return,intraday_refreshed);dev.off()

png(filename = "weights_g_mu_noise.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_g_mu, daily_return,intraday_refreshed);dev.off()

png(filename = "weights_t_noise.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_t, daily_return,intraday_refreshed);dev.off()

png(filename = "weights_t_mu_noise.png", width = 1000, height = 550)
plot_weigth_curves(weights_list = weights_t_mu, daily_return,intraday_refreshed);dev.off()
