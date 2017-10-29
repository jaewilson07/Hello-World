#install.packages("NeuralNetTools")
#install.packages("zoo", dependencies = TRUE)
#install.packages("TSA")


library('neuralnet')
library('NeuralNetTools')
library(data.table)
library(beepr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(beepr)
library(magrittr)
library(TSA)
library(xts)

wkds <- c( 'Mon', 'Tue', 'Wed' , 'Thu', 'Fri', 'Sat', 'Sun')

#import exchange and rename to friendly headings
exchange <- 
  read.csv("https://github.com/jaewilson07/Hello-World/raw/master/Datasets/DM%20and%20A/Exchange.csv") %>%
  rename( fxDate = YYYY.MM.DD, fxRate =  USD.EUR) %>%
  mutate(
    Wdy = factor(Wdy, levels = wkds, ordered = TRUE),
    fxDate = as.Date(fxDate, "%m/%d/%Y"),
    actRate = fxRate)
exchange <- setDT(exchange)

#https://stackoverflow.com/questions/33128865/starting-a-daily-time-series-in-r
str(exchange)
exchange_ts <- ts(exchange$fxRate, 
    start = c(as.numeric(format(exchange$fxDate[1], "%Y")), 
              as.numeric(format(exchange$fxDate[1], "%j"))
            ),
    frequency = 365
)

exchange_xts <- xts(exchange$fxRate, order.by = exchange$fxDate)
exchange_lr <- lm(exchange_xts[,1] ~.index(exchange_xts))
plot(exchange_xts)
abline(exchange_lr)
#the data is NOT stationary because MEAN of points (lm) is shifting (there is an upward slope in the abline)
#our variance may not be equal because distance from mean line is not the same across intervals (periods)

#Components of time series
#https://stackoverflow.com/questions/43175206/preserve-timestamp-after-decomposing-xts-in-r
#need function to convert xts object to ts object and back b/c decompose does not work with xts

decompose.xts <- function (x, type = c("additive", "multiplicative"), filter = NULL) {
  #the decompose function will break a timeseries into component parts (seasonal, trend, white noise)
  #type = if variance from mean (lm function) is steady versus multiplicative (wider swings in variance) as mean increases. 
  dts <- decompose(as.ts(x), type, filter)
    dts$x <- .xts(dts$x, .index(x))
    dts$seasonal <- .xts(dts$seasonal, .index(x))
    dts$trend <- .xts(dts$trend, .index(x))
    dts$random <- .xts(dts$random, .index(x))
    
    with(dts,
         structure(list(x = x, 
                        seasonal = seasonal, 
                        trend = trend,
                        random = if (type == "additive") x - seasonal - trend else x/seasonal/trend, 
                        class = "decomposed.xts")))
}

exchange_dc <- decompose.xts(exchange_xts, "multiplicative")

#general trend 
plot(aggregate(exchange_xts[,1], month,  FUN=mean), title("Monthly trend"))
plot(aggregate(exchange_xts[,1], year,  FUN=mean), title("Yearly trend"))

frequency(exchange_xts)
#seasonal trend