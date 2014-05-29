library(dplyr)
library(cluster)
library(klaR)
library(ggplot2)
library(scatterplot3d)
library(rgl)

 
raw_data <- read.csv("C:/Users/Fangwu Wei/Documents/Github/Repository/ST 599/Project 3/PR3q.csv")


test <- daisy(raw_data,metric="gower",stand=FALSE)
kmeans_data <- pam(test,3,diss=TRUE,metric="euclidean",stand=FALSE) 

kmeans_cluster <- raw_data %.% mutate(cluster=kmeans_data$clustering)

aggregate <- kmeans_cluster %.% group_by(cluster,q1) %.% summarise


fuzzy_data <- fanny(test,3,diss=TRUE,memb.exp=1,metric="euclidean",stand=FALSE,maxit=1000000) # endless running!!

# 2d 
dist <- cmdscale(test,2)
plot(dist,cex=0)
points(dist,col=kmeans_cluster$cluster)

# 3d
dist_3d <- cmdscale(test,3)
s3d <- scatterplot3d(dist_3d,color=kmeans_cluster$cluster)

# interactive 3d
plot3d(dist_3d,col=kmeans_cluster$cluster,size=5)

