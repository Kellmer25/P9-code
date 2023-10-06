### Dependencies 
library(tidyverse)



### Functions
sim_bm = function(start, mean, sigma, n) {
  bm = cumsum(rnorm(n, mean, sigma)) + start
  return(bm)
}

rc_estimate = function(price) {
  n = length(price)
  diff_price = diff(price)
  estimate = c()
  for (i in 1:n) {
    estimate[i] = diff_price[i] %*% t(diff_price[i])
  }
  estimate = cumsum(estimate)
  return(estimate)
}

mrc_estimate = function() {
  
}

### Input
start = 0
mean = 0
sigma = 1
n = 1000

### Main
price = sim_bm(start, mean, sigma, n)
rc = rc_estimate(price)

