#======= LOADING JMP DATA ======#

distmat <- read.csv("DistanceMatrixJMP.csv",header=TRUE,sep=",")[,-1] #distance matrix
clusters <- read.csv("ClustersJMP.csv",header=TRUE,sep=",") # data and cluster assignments

cmdcoords <- cmdscale(distmat,k=2)
all <- cbind(clusters[,1:17],cmdcoords,clusters[,18]) # coordinates cluster assignments

colnames(all) <- c(paste("q",seq(1,17,1),sep=""),"coord1","coord2","Cluster")

plot(all$coord1,all$coord2,col=as.factor(all$Cluster))

#======= JMP proportions ======#

all$q1 <- ifelse(all$q1=="republican","n","y") # commensurate coding

clustsize <- tapply(clusters[,1],clusters$Cluster,length)
clustsize


#====== for cluster 1, all questions ======#
props1 <- matrix(0,nrow=17,ncol=3)
resp <- c("y","n","?")

for (q in 1:17){
  for (i in 1:3){
    props1[q,i] <- sum(all[all$Cluster==1,q]==resp[i])/clustsize[1]
  }
}

#====== for cluster 2, all questions ======#
props2 <- matrix(0,nrow=17,ncol=3)
resp <- c("y","n","?")

for (q in 1:17){
  for (i in 1:3){
    props2[q,i] <- sum(all[all$Cluster==2,q]==resp[i])/clustsize[2]
  }
}

#====== for cluster 3, all questions ======#
props3 <- matrix(0,nrow=17,ncol=3)
resp <- c("y","n","?")

for (q in 1:17){
  for (i in 1:3){
    props3[q,i] <- sum(all[all$Cluster==3,q]==resp[i])/clustsize[3]
  }
}


props1;props2;props3


######################################
# Exporting proportions as csv files
######################################

write.table(props1,"JMPCluster1Props.csv",sep=",",quote=FALSE,row.names=FALSE)
write.table(props2,"JMPCluster2Props.csv",sep=",",quote=FALSE,row.names=FALSE)
write.table(props3,"JMPCluster3Props.csv",sep=",",quote=FALSE,row.names=FALSE)