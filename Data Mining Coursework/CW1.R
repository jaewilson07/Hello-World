install.packages('XLConnect', dependencies =  TRUE) 
require(XLConnect) # load XLConnect package 
require(NbClust)

wk = loadWorkbook("C:\\Users\\onyxr\\Dropbox (Personal)\\School\\UW\\Data Mining\\Data Mining Coursework\\whitewine.xlsx") 
df = readWorksheet(wk, sheet="White Wine")

#head(df)
#str(df)

#convert Quality into an ordinal Factor
qual_levels <- c(1:10)
df$quality	<-factor(df$quality, levels = qual_levels, ordered = TRUE)

#split the wine dataset in half for training and test
wine_train <- df[1:nrow(df)/2, ]
wine_test <-  df[nrow(df)/2:nrow(df), ]

#isolate the quality columns
train_quality <- wine_train$quality
test_quality <- wine_test$quality

#create a function for scaling variables on a 0:1 scale
min_max <- function(x) {
		(x - min(x)) / ( max(x) - min(x))
	}

#apply min_max to each column of the data set
for (i in 1:(ncol(wine_train)-1) ){ wine_train[i] <- min_max(wine_train[i]) }
for (i in 1:(ncol(wine_test)-1) ){ wine_test[i] <- min_max(wine_test[i]) }

for (i in 1:5 { 
	bestClust[i] <- NbClust( sample(wine_train[1:11], 100, replace=FALSE), method = "kmeans", min.nc =  2, max.nc = 15)

})

plot(bestClust$Best)

str(bestClust)

print("hello world")	