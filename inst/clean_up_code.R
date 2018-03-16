data <- read.table("data/mortality.dat")
colnames(data) <- c('service', 'years', 'censor')
head(data)

salloon <- data$service == 1
rest <- data$service == 2
exp <- data$service == 3
cen_yes <- data$censor == 1
cen_no <- data$censor == 0

data[salloon, 1] <- "salloon"
data[rest, 1] <- "rest"
data[exp, 1] <- "exp"

data[cen_yes, 3] <- "censored"
data[cen_no, 3] <- "interval"


write.csv(data, file = './data/mortality_data.csv')
