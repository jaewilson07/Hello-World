install.packages("astsa")
install.packages("forecast")
library("astsa")
library("tsplot")
library("forecast")
set.seed(154)

i = .2
# generate a random number
w = rnorm(200)

#add a fixed value to rnorm()
wd = w + i

#take cumulative sum of rnorm()
xd = cumsum(wd)
tb <- data.frame(w, wd, xd)
head(tb,45)

ts.plot(xd, ylim = c(-5,55), main = "random walk", ylab = '')

#add a trendline based on slope i
#lty line type dashed or not
#http://www.sthda.com/english/wiki/line-types-in-r-lty
abline(a=0, b= i , lty = 2)

#take cumulative sum of w (rnorm())
x = cumsum(w)
lines(x, col=3)
abline(h=0, col=3, lty=2)

#Stationary Time Series
# -- mean value function, u(t), is constant and does not depend on time
# -- variance (distance from mean) should be equal at equal intervals of time.
# -- covariance should be equal

# -- 3point moving average is stationary b/c mean and autocovariance are INDEPENDENT Of time
# -- Trend Stationarity


#chicken is a time series plot of 180 points
chicken <-chicken
#create a linear regression (linear trend line) of chicken data
summary(fit <- lm(chicken~time(chicken)))

#plot chicken
ts.plot(chicken, ylab = "cents per pound")
#plot the trendline
abline(fit)


df <- cbind(cmort, tempr, part)
str(df)
ts.plot(cmort, main="cardiovascular Mortality", ylab ="")
ts.plot(tempr, main="Temperature", ylab = "")
ts.plot(part, main = "Particulates", ylab="")

ts.plot(df[,1], df[,2], df[,3], col = 1:3)

#plot pairs of values
pairs(df)
temp <- tempr - mean(tempr) #center (error)
temp2 <- temp^2 # (error squared)
trend <- time(cmort)
fit <- lm(cmort ~trend + temp + temp2 + part, na.action = NULL)
summary(fit)
summary(aov(fit))
summary(aov(lm(cmort~cbind(trend, temp, temp2, part))))
num = length(cmort)        

AIC(fit)/num - log(2*pi)
BIC(fit)/num - log(2*pi)
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2))

##Chicken
chicken_lr = lm(chicken~time(chicken), na.action=NULL) # regress chicken on time
par(mfrow=c(3,1))

ts.plot(chicken) #original data
#analyze the effect of linear trend
abline(chicken_lr) #plot linear regression trendline
chicken_resid <- resid(chicken_lr) #where residuals are the difference between trend line and actual (x-xbar)
ts.plot(chicken_resid, main="detrended") #the actual data with linear regression trend removed

#plot the effect of previous day trend
ts.plot(diff(chicken), main="first difference") #calculate one day lag (cur day minus previous day)


#ACF tells how correlated points are based on by how many time steps they are separated
#https://stats.stackexchange.com/questions/77248/what-is-autocorrelation-function
#acf = 1 = correlation of 1
par(mfrow=c(3,1)) # plot ACFs

acf(chicken, 48, main="chicken")
acf(resid(fit), 48, main="detrended")
acf(diff(chicken), 48, main="first difference")

