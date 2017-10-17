library(ggplot2)
#create a dataframe of sample values
x <- c(7,3,4,6,10,9)
y <- c(276,43,82,136,417,269)
my_variable <- data.frame( x, y)	

#create a model as a linear regression data frame my_variable
model <- lm(y~x, data = my_variable)

model$coef
#plot the points
#add
ggplot(data = model, aes( x=x , y=y)) +
	geom_point() +
	geom_abline(
			intercept = model$coef[1], 
			slope = model$coef[2], color = "red")

summary(model)

#create a sample of 1000 values with a mean 5 and SD 7
x_1 <- rnorm(1000, 5, 7)
hist(x_1, col="grey")

true_error <- rnorm(1000,0, 2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2

y<- true_beta_0 + true_beta_1 * x_1 + true_error

hist(y)
plot(x_1, y, pch = 20, col="red")

x_1df <- data.frame("x" = x_1, y)
str(x_1df)
model <- lm(y ~x, data = x_1df)

ggplot(x_1df, aes(x = x, y = y)) +
	geom_point()

#does coef(model) return true_beta0 and true_beta1?
coef(model)

x_2 <- rgamma(n= 1000,shape=2)
y2 <- true_beta_0 + true_beta_1 * x_2 + true_error
x_2df <- data.frame(x = x_2, y = y2)
model2 <- lm(y~x , x_2df)