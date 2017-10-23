#install.packages("ChemometricsWithR", dependencies = TRUE)

library(dendextend)
library(magrittr)
library(colorspace)
library(gplots)
library(corrplot)
library(ChemometricsWithR)
library(cluster)
library(e1071)


mtcars_df <- mtcars

#calculate distance between variables
#do we need to scale (mtcars?)
mtcars_dist <- dist(mtcars_df, method = "euclidean")

#cluster mtCars
mtcars_hc <- hclust(mtcars_dist, method = "complete")

#print Dendogram
plot(mtcars_hc , main = "Hierarchical cluster from mtcars dataset")

#draw dendogram w. red borders
rect.hclust(mtcars_hc, k = 3, border = "red")

#cutree and find mean values of variables across cluster
#what are the mean values of variables across groups?
groups <- cutree(mtcars_hc, k = 3)
table(groups)
round(aggregate(mtcars_df, FUN = mean, by = list(groups)), 1)


#clustering with iris dataset
iris_2 <- iris[,-5]
iris_labels <- iris[,5]
iris_dist <- dist(iris_2, method = "euclidean")
iris_hc <- hclust(iris_dist, method = "complete")

#create a list of factor values in iris$species
iris_species <- rev(levels(iris_labels))

#plot the iris dendogram with k=3 clusters
  as.dendrogram(iris_hc) %>% 
  rotate(1:150) %>%
  color_branches(k = 3) %>%
  hang.dendrogram(hang_height = .1) %>%
  set( "labels_cex", 0.5) ->
iris_dend

  #add labels
#color by known output value membership (note cluster size must be equal to number of predictor factors)
iris_dend %<>% set("labels_colors", rainbow_hcl(3)[sort_levels_values(as.numeric(iris_labels)[order.dendrogram(iris_dend)])])
iris_dend %<>% set("labels", paste( as.character(iris_labels)[order.dendrogram(iris_dend)], "(" ,labels(iris_dend), ")", sep = "'" ))

par(mar = c(3,3,3,7))
plot(iris_dend,
      main= "Clustered Iris data set" ,
     horiz = TRUE,nodePar = list(cex = .007))
legend("topleft", legend = iris_species , fill= rainbow_hcl(3))

#create a heatmap of Iris dendogram 
some_col_func <- function(x) rev(
  heat_hcl(x, 
    c = c(80,30), 
    l = c(30,90), 
    power = c( .2 , 1.50))
  )

heatmap.2(as.matrix(iris_2),
          main = "Heatmap for Iris Data Set" ,
          srtCol = 20 ,
          dendrogram = "row" ,
          Rowv = iris_dend ,
            Colv = "NA" ,
          trace = "none" ,
          margins = c(5, 0.1) ,
          key.xlab = "Cm",
          denscol = "grey" ,
          density.info = "density" ,
          RowSideColors =  rev(labels_colors(iris_dend)) , 
          col = some_col_func
  )

#how similar are the clusters produced by different hclust methods?

#identify the hclust methods to run
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", 
                    "median", "centroid", "ward.D2")

#store hclust() objects for each cluster method
iris_dendlist <- dendlist()
  for( i in seq_along(hclust_methods)) {
    hc_iris <- hclust( iris_dist , method = hclust_methods[i] )
    iris_dendlist <- dendlist( iris_dendlist , as.dendrogram(hc_iris))
  }

names(iris_dendlist) <- hclust_methods
iris_dendlist

#find the correlation in membership between clustering methods
iris_dendlist_cor <- cor.dendlist(iris_dendlist)
iris_dendlist_cor

corrplot(iris_dendlist_cor, "pie", "lower")


wines_df <- read.csv( "https://github.com/jaewilson07/Hello-World/raw/master/Datasets/DM%20and%20A/Whitewine.csv", header= TRUE)
wines_sample <- sample(nrow(wines_df), 20)

wines_dist_sample <- dist(wines_df[wines_sample,], method = "euclidean")
wines_dist <- dist(wines_df, method = "euclidean")

wines_hc_sing_sample <- hclust(wines_dist_sample, method = "single")
wines_hc_complete_sample  <- hclust(wines_dist_sample, method = "complete")
wines_hc_ward_sample  <- hclust(wines_dist_sample, method = "ward.D")

cut_hc_sing_sample <- cutree(wines_hc_sing_sample , k = 5)
cut_hc_ward_sample <- cutree(wines_hc_ward_sample , k = 5)
cut_hc_compl_sample <- cutree(wines_hc_complete_sample, k=5)

#table(vintages, cut_hc_sing)

wines_agnes_sing <- agnes( wines_dist , method = "single")
wines_agnes_compl <- agnes( wines_dist , method = "complete")
wines_agnes_avg <- agnes( wines_dist , method = "average")

cbind(wines_agnes_sing$ac , wines_agnes_avg$ac , wines_agnes_compl$ac )

result <- cmeans( wines_dist, 3, 100, method= "cmeans" )

plot(wines_df, col = result$cluster)

result$membership[1:3,]
