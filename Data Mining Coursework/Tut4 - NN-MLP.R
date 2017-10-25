#install.packages('neuralnet', dependencies = TRUE)
install.packages('NerualNetTools' , dependencies = TRUE)
install.packages("datasets")

library('neuralnet')
library('NeuralNetTools')
library('datasets')

#cretae a dataset of random numbers and their sqrrt.
#generate numbers based on normal distribut8ion runif
traininginput <- as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)
trainingdata <- cbind(traininginput, trainingoutput)
colnames(trainingdata) <- c("Input", "Output")
head(trainingdata)


#preparing data for neurla network:
# normalize the data on a scale of 0:1
#number of neurons should be btwn input layer size and output 
  #input layer is the number of input variables
#train neural network w/ 10 hidden layers
#threshold = numeric value specifiying theshold for the partial derivatives of error function as stopping criteria
net_sqrt <- neuralnet(Output ~ Input, trainingdata, hidden = 10, threshold = 0.01)
ls(net_sqrt)

#either model will work for binding net.result back to the training data
trainingdata <-cbind(trainingdata, as.data.frame(unlist(net_sqrt$net.result)))
#trainingdata$actual <- as.numeric(unlist(net_sqrt$net.result))

head(trainingdata)
str(trainingdata)
print(net_sqrt)
plot(net_sqrt)

#generate test data
testdata <- as.data.frame((1:10)^2)

#pass testdata through the net_sqrt nerualnet
net_results <- compute(net_sqrt, testdata)

ls(net_results)

print(net_results$net.result)
cleanoutput <- cbind(cleanoutput, as.data.frame(net_results$net.result))
colnames(cleanoutput) <- c("Input", "Actual", "NN Est")
head(cleanoutput)

concrete <- read.csv("https://github.com/jaewilson07/Hello-World/raw/master/Datasets/DM%20and%20A/concrete.csv")

#minmax normailzation function
minmax <- function(x) {
    return((x- min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame( sapply(concrete, minmax))
summary(concrete_norm$strength)

concrete_train <- concrete_norm[1:773,]
concrete_test  <- concrete_norm[774:1030,]

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + agg, data = concrete_train )

#SSE (sum of squared Predicted minus Actual Values)
plot(concrete_model)
plotnet(concrete_model, alpha = .6)

#note do not pass in strength (column 9)
#compute stores two components 
  #$neurons (neurons for each layer)
  #$net.result, predicted values
model_results <- compute(concrete_model, concrete_test[,1:8])

predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength)

#has one layer with 5 neurons on 1 hidden layer
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + agg, data = concrete_train, hidden=5)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

names(infert)

#err.fct ="ce"  for use if response variable is binary / where ce = cross entropy
#linear.output = FALSE ensure output mapped by the activation function to the interval [0, 1]
nn<-neuralnet(case ~ age + parity + induced + spontaneous, data = infert, hidden = 2, err.fct = "ce", linear.output = FALSE)

#activation function:  sigmoid = function(x) { 1 / ( 1 + exp(-x)) }
# nn2 <- neuralnet( case ~ parity + induced + spontaneous, infert, err.fct = "ce" , linear.output = FALSE, likelihood = TRUE, act.fct = sigmoid)

nn$result.matrix

out <- cbind(nn$covariate , nn$net.result[[1]]] )
  
dimnames(out) <- list( NULL, c("age", "parity", "induced", "spontaneous", "nn-output"))
head(out)

head(nn$generalized.weights[[1]])
plot(nn)
