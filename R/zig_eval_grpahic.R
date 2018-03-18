#n is total number of obs wanted

q <- rbinom(1225, 1, .7144151)
holding <- rnbinom(sum(q), 1, .1561350)
holding <- ifelse(holding > 10, 11, holding)
fails <- rep(0, 1225 - sum(q))
fake_zero_inf <- c(holding, fails)
length(fake_zero_inf)

ggplot(as.data.frame(fake_zero_inf), aes(fake_zero_inf)) + geom_histogram()
library(ggplot2)
