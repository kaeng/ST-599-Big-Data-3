###############################################################################
# RUN FOLLOWING CODE ONLY IF YOU NEED THE CONCORDANCE DISTANCE MATRIX
###############################################################################

#======= using klaR's kmodes to get dist matrix ======#
# library(klaR)
# 
# data <- read.csv("PR3.csv",header=TRUE)
# 
# # get cluster assignments
# cluster3 <- kmodes(data,modes=3)
# 
# # trying to get a distance matrix
# #sum(data[1,]!=data[2,]) # this is how many don't match
# 
# # super slow distance matrix counting differences... but oh well
# distmatkmodes <- matrix(0,nrow=435,ncol=435)
# for (i in 1:435){
#   for (j in 1:435){
#     distmatkmodes[i,j] <- sum(data[i,]!=data[j,])
#   }
#   print(i)
# }
# 
# write.table(distmatkmodes,"concordancematrix.csv",sep=",",quote=FALSE,row.names=FALSE)


####################################
# START RUNNING CODE HERE, Loading concordance distance matrix
####################################

library(klaR)

data <- read.csv("PR3.csv",header=TRUE)

# get cluster assignments
cluster3 <- kmodes(data,modes=3)

################
# here
################
distmatkmodes <- read.csv("concordancematrix.csv")

# ====== getting plot with disTance matrix loaded, distmatkmodes object here ==== #

cmdcoordskmodes <- cmdscale(distmatkmodes,k=2)

## ALL Q'S, COORDS, CLUSTER ASSIGNMENTS
allkmodes <- cbind(data,cmdcoordskmodes,cluster3$cluster)

colnames(allkmodes) <- c(paste("q",seq(1,17,1),sep=""),c("coord1","coord2","Cluster"))

allkmodes$q1 <- ifelse(allkmodes$q1=="republican","n","y") # commensurate coding

plot(allkmodes$coord1,allkmodes$coord2,col=as.factor(allkmodes$Cluster))


clustsize <- tapply(allkmodes[,1],allkmodes$Cluster,length)
clustsize

#====== for cluster 1, all questions ======#
props1 <- matrix(0,nrow=17,ncol=3)
resp <- c("y","n","?")

for (q in 1:17){
  for (i in 1:3){
    props1[q,i] <- sum(allkmodes[allkmodes$Cluster==1,q]==resp[i])/clustsize[1]
  }
}

#====== for cluster 2, all questions ======#
props2 <- matrix(0,nrow=17,ncol=3)
resp <- c("y","n","?")

for (q in 1:17){
  for (i in 1:3){
    props2[q,i] <- sum(allkmodes[allkmodes$Cluster==2,q]==resp[i])/clustsize[2]
  }
}

#====== for cluster 3, all questions ======#
props3 <- matrix(0,nrow=17,ncol=3)
resp <- c("y","n","?")

for (q in 1:17){
  for (i in 1:3){
    props3[q,i] <- sum(allkmodes[allkmodes$Cluster==3,q]==resp[i])/clustsize[3]
  }
}


props1;props2;props3

######################################
# Exporting proportions as csv files
######################################

write.table(props1,"KModesCluster1Props.csv",sep=",",quote=FALSE,row.names=FALSE)
write.table(props2,"KModesCluster2Props.csv",sep=",",quote=FALSE,row.names=FALSE)
write.table(props3,"KModesCluster3Props.csv",sep=",",quote=FALSE,row.names=FALSE)
