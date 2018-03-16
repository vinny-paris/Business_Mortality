




fake1 <- rnbinom(175*9, 1, .1281556)
fake1 <- ifelse(fake1 > 10, 11, fake1)
fake1 <- data.frame(fake1, kronecker(1:9, rep(1, 175)))
fake1$service <- rep("exp", 175)
colnames(fake1) <- c('data', 'group', "service")



fake2 <- rnbinom(409*9, 1, .2283161)
fake2 <- ifelse(fake2 > 10, 11, fake2)
fake2 <- data.frame(fake2, kronecker(1:9, rep(1, 409)))
fake2$service <- rep("rest", 409)
colnames(fake2) <- c('data', 'group', 'service')


fake3 <- rnbinom(641*9, 1, .2249506)
fake3 <- ifelse(fake3 > 10, 11, fake3)
fake3 <- data.frame(fake3, kronecker(1:9, rep(1, 641)))
fake3$service <- rep('salloon', 641)
colnames(fake3) <- c('data', 'group', 'service')

fake <- rbind(fake1, fake2, fake3)


par(mfrow = c(3,3))

ggplot(fake, aes(data)) + geom_histogram(aes(fill = as.factor(service))) + facet_wrap( ~ group )

ggplot(data, aes(years)) + geom_histogram(aes(fill = service)) 
