



loglik <- function(data, par){
  
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
  
  
  
  #Log-likelihood of geometric distribution with parameter p and variable k
  lnL1 <- sum(  d1 * ((k1-1) * log(1-p_exp) + log(p_exp))  +  (1-d1) * 10 * log(1-p_exp))
             
  lnL2 <- sum(  d2 * ((k2-1) * log(1-p_rest) + log(p_rest))  +  (1-d2) * 10 * log(1-p_rest))
             
  lnL3 <- sum(  d3 * ((k3-1) * log(1-p_salloon) + log(p_salloon))  +  (1-d3) * 10 * log(1-p_salloon))
  

lnL <- sum(lnL1, lnL2, lnL3)
  
  return(-(lnL))
  
}


result <- optim(par = c(.5, .5, .5), loglik, data = data, hessian = T, method = 'L-BFGS-B', lower = 0.001, upper = .999)

result
