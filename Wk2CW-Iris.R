if( require(cluster)) {
  print("cluster is loaded correctly")
} else {
  print("trying to install cluster")
  install.packages("cluster")
  if(require(cluster)){
    print("cluster installed and loaded")
  } else {
    stop("could not install cluster")
  }
}	

df_iris <- iris
iris_var <- iris[,1:4]
iris_tar <- iris[,5]

iris_km <- kmeans(iris_var, 3)
iris_km_err <- table(iris_tar, iris_km$cluster)

iris_pm <- pam(iris_var, 3)
iris_pm_err <- table(iris_tar, iris_pm$clustering)

layout(matrix(c(1,2),1,2))
plot(iris_pm)