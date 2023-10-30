library(ggplot2)
library(dplyr)
library(highfrequency)
library(data.table)
library(glue)

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
  observed = rep(FALSE, ncol(df))
  times = c(max(df[1, ]))
  
}

set.seed(12311)
df = obs_times(N=10, lambdas=c(1,2,3), df=TRUE)

ggplot(df) + 
  geom_point(aes(x=lambda1, y=1), col="blue") + 
  geom_point(aes(x=lambda2, y=2), col="red") + 
  geom_point(aes(x=lambda3, y=3), col="green") + 
  scale_x_continuous(breaks=) + 
  labs(x="Time", y = "Asset")

