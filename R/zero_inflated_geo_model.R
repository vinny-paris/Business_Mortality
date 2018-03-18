
loglik_zero <- function(data, par){
  
  k <- data$years
  d <- data$censor == 'interval'
  p <- par[1]
  z <- par[2]
  j <- k == 1
  
  lnL <- sum(j*log((1-z) + z*p) + (1-j) * (d*(log(z) + (k-1)*log(1-p) + log(p)) + (1-d)*10*log(1-p)))
  
  return(-lnL)
  
}

data <- read.csv('./data/mortality_data.csv')[,-1]
result <- optim(par = c(.5, .5), loglik_zero, data = data, hessian = T, method = 'L-BFGS-B', lower = 0.001, upper = .999)

result
