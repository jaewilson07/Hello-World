if( require('rattle')) {
  print("rattle is loaded correctly")
} else {
  print("trying to install rattle")
  install.packages("rattle")
  if(require(rattle)){
    print("rattle installed and loaded")
  } else {
    stop("could not install rattle")
  }
}	

if( require('NbClust')) {
  print("NbClust is loaded correctly")
} else {
  print("trying to install NbClust")
  install.packages("NbClust")
  if(require(NbClust)){
    print("NbClust installed and loaded")
  } else {
    stop("could not install NbClust")
  }
}

if( require('rattle.data')) {
  print("rattle.data is loaded correctly")
} else {
  print("trying to install rattle.data")
  install.packages("rattle.data")
  if(require(rattle.data)){
    print("rattle.data installed and loaded")
  } else {
    stop("could not install ratle.data")
  }
}

if( require('fpc')) {
  print("fpc is loaded correctly")
} else {
  print("trying to install fpc")
  install.packages("fpc")
  if(require(fpc)){
    print("fpc installed and loaded")
  } else {
    stop("could not install fpc")
  }
}

install.packages("flexclust")
require('MASS')
require(rattle.data)
require(flexclust)


df_wine <- wine
df_wine_scale <- scale(wine[,-1])

nclust_wine <- NbClust(df_wine_scale, min.nc = 2 , max.nc = 15, method="kmeans")

barplot(table(nclust_wine$Best.n[1,]),
	xlab = "Number of Clusters",
	ylab = "Nmber of Criteria",
	main = "Number of Clusters Chosen by 26 Criteria")

plot(nclust_wine)

wine_km <- kmeans(df_wine_scale, 3)
wine_km_err <- table(wine$Type, wine_km$cluster)
wine_km_err

randIndex(wine_km_err)

plotcluster(df_wine_scale, wine_km$cluster)
parcoord(df_wine_scale, wine_km$cluster)