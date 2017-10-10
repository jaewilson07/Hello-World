#install.packages('XLConnect', dependencies =  TRUE) 
require(XLConnect) # load XLConnect package 
require(NbClust)

wk <- loadWorkbook("C:\\Users\\onyxr\\Dropbox (Personal)\\School\\UW\\Data Mining\\Data Mining Coursework\\whitewine.xlsx") 
wine <- readWorksheet(wk, sheet="White Wine")


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

table(wine[,12])
table(k_wine_10$cluster, wine[,12])
table(k_wine_1$cluster, wine[,12])
table(k_wine_2$cluster, wine[,12])
