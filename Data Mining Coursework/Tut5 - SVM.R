#SVM iniitally developed as a CLASSIFIER (linear / nonlinear)
#SVM can solve classification and regression problems

#minstall.packages("e1071")
library("e1071")
library("dplyr")
library("tictoc")

n = 125
  
iris <- iris
iris_train <- iris[1:n]
iris_train_pred <- iris[1:n, 5]

iris_test <- iris[ (n+1): nrow(iris), -5]

str(iris)
str(iris_train)
str(iris_train_pred)
str(iris_test)

iris_svm <- svm(Species ~ . , data = iris)

iris_svm2 <- svm(iris_train, iris_train_pred)

tic("svm_pred")
iris_svm_pred <- predict(iris_svm, iris_train)
(toc_pred <- toc())

table(iris_svm_pred, iris_train_pred)

iris_svm_tune <- tune(svm, train.x = iris_train, 
                      train.y = iris_train_pred,
                      kernel = "radial", 
                      ranges = list(cost = 10^ (-1:2), gamma = c(.5,1,2)))
summary(iris_svm)
summary(iris_svm2)
summary(iris_svm_tune)     
print(iris_svm)
print(iris_svm2)
print(iris_svm_tune)

iris_svm2_tune <- svm(Species ~., data = iris_train, kernel = "radial"
                      , cost =1 
                      , gamma = .5)
