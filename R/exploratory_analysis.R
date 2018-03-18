data <- read.csv('./data/mortality_data.csv', header = T)[,-1]
head(data)

library(ggplot2)

ggplot(data, aes(years)) + geom_histogram(aes(fill = service)) + facet_grid(~service)
ggplot(data, aes(years)) + geom_histogram(aes(fill = service))
tab <- table(data[,1:2])
totals <- apply(tab, 1, sum)
survived <- tab[,11]

#Percent survived
survived/totals

ratios <- NULL
holding <- NULL
for(j in 1:3){
 for(i in 1:9){
  holding[i] <- tab[j,i+1] / tab[j,i]
 }
 ratios <- cbind(ratios, holding)
}

colnames(ratios) <- c("exp", "rest", "salloon")
rownames(ratios) <- c("2/1", "3/2", "4/3", "5/4", "6/5", "7/6", "8/7", "9/8", "10/9")






x <- fa <- 2 <- d <- 3
