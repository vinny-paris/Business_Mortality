
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



loglik_zero_exp <- function(data, par){

data1 <- subset(data, data$service == 'exp')
k1 <- data1$years
d1 <- data1$censor == 'interval'

data2 <- subset(data, data$service == 'rest')
k2 <- data2$years
d2 <- data2$censor == 'interval'

data3 <- subset(data, data$service == 'salloon')
k3 <- data3$years
d3 <- data3$censor == 'interval'


p_exp <- par[1]
p_rest <- par[2]
p_salloon <- par[3]
z_exp <- par[4]
z_rest <- par[5]
z_salloon <- par[6]

j1 <- k1 == 1
j2 <- k2 == 1
j3 <- k3 == 1


lnL1 <- sum(j1*log((1-z_exp) + z_exp*p_exp) + (1-j1) * (d1*(log(z_exp) + (k1-1)*log(1-p_exp) + log(p_exp)) + (1-d1)*10*log(1-p_exp)))

lnL2 <- sum(j2*log((1-z_rest) + z_rest*p_rest) + (1-j2) * (d2*(log(z_rest) + (k2-1)*log(1-p_rest) + log(p_rest)) + (1-d2)*10*log(1-p_rest)))

lnL3 <- sum(j3*log((1-z_salloon) + z_salloon*p_salloon) + (1-j3) * (d3*(log(z_salloon) + (k3-1)*log(1-p_salloon) + log(p_salloon)) + (1-d3)*10*log(1-p_salloon)))


lnL <- sum(lnL1, lnL2, lnL3)

return(-(lnL))
}

result <- optim(par = c(.5, .5, .5, .5, .5, .5), loglik_zero_exp, data = data, hessian = T, method = 'L-BFGS-B', lower = 0.001, upper = .999)

result



