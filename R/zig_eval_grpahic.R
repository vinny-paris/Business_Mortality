#n is total number of obs wanted

q <- rbinom(1225, 1, .7144151)
holding <- rnbinom(sum(q), 1, .1561350)
holding <- ifelse(holding > 10, 11, holding)
fails <- rep(0, 1225 - sum(q))
fake_zero_inf <- c(holding, fails)
length(fake_zero_inf)

ggplot(as.data.frame(fake_zero_inf), aes(fake_zero_inf)) + geom_histogram()
library(ggplot2)





q1 <- rbinom(175*9, 1, .7910311)
fake1 <- rnbinom(sum(q1), 1, .1041749)
fake1 <- ifelse(fake1 > 10, 11, fake1)
fails1 <- rep(0, 175*9 - sum(q1))
fake1 <- c(fake1, fails1)
fake1 <- data.frame(fake1, kronecker(rep(1, 175), 1:9))
fake1$service <- rep("exp", 175)
colnames(fake1) <- c('data', 'group', "service")


q2 <- rbinom(409*9, 1, .7279662)
fake2 <- rnbinom(sum(q2), 1, .1734933)
fake2 <- ifelse(fake2 > 10, 11, fake2)
fails2 <- rep(0, 409*9 - sum(q2))
fake2 <- c(fake2, fails2)
fake2 <- data.frame(fake2, kronecker(rep(1, 409), 1:9))
fake2$service <- rep("rest", 409)
colnames(fake2) <- c('data', 'group', 'service')

q3 <- rbinom(641*9, 1, .6967981)
fake3 <- rnbinom(sum(q3), 1, .1682211)
fake3 <- ifelse(fake3 > 10, 11, fake3)
fails3 <- rep(0, 641*9 - sum(q3))
fake3 <- c(fake3, fails3)
fake3 <- data.frame(fake3, kronecker(rep(1, 641), 1:9))
fake3$service <- rep('salloon', 641)
colnames(fake3) <- c('data', 'group', 'service')


fake <- rbind(fake1, fake2, fake3)

ggplot(fake, aes(data)) + geom_histogram(aes(fill = as.factor(service))) + facet_wrap( ~ group )

ggplot(data, aes(years)) + geom_histogram(aes(fill = service)) 