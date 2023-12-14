library(ggplot2)
library(dplyr)
library(data.table)
library(glue)
library(xts)
library(viridis)

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

refresh_times = function(df) {
  tickers = colnames(df)
  observed = list()
  for (ticker in tickers) {
    observed[[ticker]] = df[1, ticker]
  }
  
  times = c()
  for (i in 1:nrow(df)) {
    row = df[i, ]
    test = unlist(observed)
    for (ticker in tickers) {
      if (isFALSE(observed[[ticker]])) {
        if (row[ticker] >= times[length(times)]) {
          observed[[ticker]] = row[ticker]
        }
      }
    }
    if (all(sapply(observed, function(x) x != FALSE))) {
      test = unlist(observed)
      obs_time = max(test)
      times = c(times, obs_time)
      observed = lapply(observed, function(x) FALSE)
    }
  }
  return(times)
}

set.seed(12311)
df = obs_times(N=10, lambdas=c(1,2,3), df=TRUE)
times = refresh_times(df)

ggplot(df) + 
  geom_point(aes(x=lambda1, y=1), color="#b5de2c", size=3) + 
  geom_point(aes(x=lambda2, y=2), color="#1fa187", size=3) + 
  geom_point(aes(x=lambda3, y=3), color="#472f7d", size=3) +
  geom_vline(xintercept=times, linetype="dashed", linewidth=0.8) + 
  scale_x_continuous(breaks=seq(1, 20, 1)) + 
  scale_y_continuous(breaks=c(1,2,3), limits=c(0.5,3.5)) +
  theme(panel.border = element_blank()) + 
  labs(x="Time", y = "Lambda") + 
  scale_fill_viridis() +
  theme_minimal()

