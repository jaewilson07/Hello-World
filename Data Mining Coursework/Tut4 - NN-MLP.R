#install.packages('neuralnet', dependencies = TRUE)

library('neuralnet')

#generate numbers based on normal distribut8ion runif

traininginput <- as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

trainingdata <- cbind(traininginput, trainingoutput)
colnames(trainingdata) <- c("Input", "Output")

head(trainingdata)

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

