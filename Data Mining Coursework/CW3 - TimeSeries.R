
# use 80:20 training / test
#how many inputs do you need? GOOGLE Time Series problem

library(dplyr)
exchange <- read.csv("https://github.com/jaewilson07/Hello-World/raw/master/Data%20Mining%20Coursework/Exchange.csv")

#write function to scale data from 0:1 scale
scale0 <- function(x) { (x - min(x) ) / ( max(x) - min(x) )}

#apply scale function to data
exchange %>%
mutate(
	USDscale = scale0(USD.EUR)
	) ->
exchange

#calculate nth row in data set to partition training and test data
split_train <- nrow(exchange) * .8

#create training and test partitions
exchange_train <- exchange [1:split_train, ]
exchange_test  <- exchange[ (split_train+1):nrow(exchange) , ]

