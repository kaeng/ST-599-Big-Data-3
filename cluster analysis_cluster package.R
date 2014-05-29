library(dplyr)
library(cluster)
library(klaR)
library(ggplot2)
library(scatterplot3d)
library(rgl)

 
raw_data <- read.csv("C:/Users/Fangwu Wei/Documents/Github/Repository/ST 599/Project 3/App-1/Data/PR3q.csv")

# First, using function "daisy" in "cluster" package to generate dissimilarity matrix in terms of "gower" method
# "Gower" method is used to measure categorical data and simply compare two records. If they are the same, give the value 1. If not, give the value 0.
test <- daisy(raw_data,metric="gower",stand=FALSE)

# Second, using function "pam" in "cluster" package to do a k-means clustering analysis.
# The analysis results include silhouette. A parameter validates the clusters of data, ranging from 0 to 1. Closer 1, better the partition of data is.
# Tried different number of clusters and found 3 would be the best.
# Local optimal solution is always an issue for k-means clustering method.
kmeans_data <- pam(test,3,diss=TRUE,metric="euclidean",stand=FALSE) 

kmeans_cluster <- raw_data %.% mutate(cluster=kmeans_data$clustering)

aggregate <- kmeans_cluster %.% group_by(cluster,q1) %.% summarise


# Fuzzy clustering is used to calculate possibility of each observation in each cluster.
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


