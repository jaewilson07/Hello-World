<<<<<<< HEAD
#install.packages('XLConnect', dependencies =  TRUE) 
#install.packages('tictoc')
#install.packages('beepr')
#install.packages('flexclust')
#install.packages('corrplot')
#install.packages('dendextend')

require(XLConnect) # load XLConnect package 
require(NbClust)
require(tictoc)
require(beepr)
require(flexclust)
require(corrplot)
require(dendextend)

wk <- loadWorkbook("d:\\users\\onyxr\\Dropbox (Personal)\\School\\UW\\Data Mining\\Data Mining Coursework\\whitewine.xlsx") 
wine <- readWorksheet(wk, sheet="White Wine")

print("hello")

#head(df)
#str(wine)

#convert Quality into an ordinal Factor
qual_levels <- c(1:10)
wine$quality	<-factor(wine$quality, levels = qual_levels, ordered = TRUE)

#isolate the quality columns
wine_quality <- wine$quality

#create a function for scaling variables on a 0:1 scale
wine_train <- wine[,-12]
wine_train <- scale(wine_train)

#find recommendation for best number of clusters
tic()
numClust_wine <- NbClust(wine_train, method = "kmeans", min.nc =  2, max.nc = 15)
beep()
t_clust<- toc()

table(numClust_wine$Best.n[1,])

#find the two most strongly recomended number of clusters
#create a dataframe containing recommended clusters sizes and the count of Indexes that recomended them
#recast Var1 (the recomended cluster size) as an integer
recClust_wine <-data.frame(table(numClust_wine$Best.n[1,]))
str(recClust_wine)
recClust_wine$Var1 <- as.integer(recClust_wine$Var1)

#find the two most strongly recomended cluster sizes (based on index count)

topCLust_wine_Rec = data.frame()

repeat {
  topCLust_wine_Rec <- rbind(topCLust_wine_Rec, recClust_wine[recClust_wine$Freq == max(recClust_wine$Freq),])
  recClust_wine <- recClust_wine[!recClust_wine$Freq == max(recClust_wine$Freq),]
  print(nrow(topCLust_wine_Rec))
  if (nrow(topCLust_wine_Rec) >= 2) {break}
}

print(paste("Run k-means using 10, the number of actual Quality groups.  Additionally run k-means with cluster sizes..."))
print(topCLust_wine_Rec$Var1 )

k_wine_10 <- kmeans(wine_train, 10)
k_wine_1 <- kmeans(wine_train, topCLust_wine_Rec$Var1[1])
k_wine_2 <- kmeans(wine_train, topCLust_wine_Rec$Var1[2])

tbwine <- table(wine[,12])
tb_ct_10 <- table(k_wine_10$cluster, wine[,12])
tb_ct_1 <- table(k_wine_1$cluster, wine[,12])
tb_ct_2 <- table(k_wine_2$cluster, wine[,12])

randIndex(tb_ct_10)
randIndex(tb_ct_1)
randIndex(tb_ct_2)


#hclust (agglomerative) single
hclust_sing <- hclust( dist(wine_train), method= "single")
dend_sing <- as.dendrogram(hclust_sing)
#hclust complete
hclust_compl <- hclust( dist(wine_train), method= "complete")
dend_compl <- as.dendrogram(hclust_compl)
#hclustaverage methods
hclust_avg <- hclust( dist(wine_train), method= "average")
dend_avg <- as.dendrogram(hclust_avg)

#Create dendogram visualization of all 3
layout(matrix(c(1:3),1,3))
plot(hclust_sing)
plot(hclust_compl)
plot(hclust_avg)

#create a list of dendgrams
d <- dendlist(
		single = dend_sing,
		complete = dend_compl,
		average = dend_avg)
#cor.dendlist between each clustering result 
cor.dendlist(d)
beep()
#Discuss the produced results after using the coorplot function.

str(hclust_avg)

str()
h<- dendlist(hclust_avg)
=======
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
>>>>>>> 14fec27bed41f3c50cf3eec495059926b2df78a4
