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



################################
# Proportion heat maps
################################
library(ggplot2)
library(ggvis)

# load proportions if necessary
# props1 <- read.csv("KModesCluster1Props.csv",header=TRUE)
# props2 <- read.csv("KModesCluster2Props.csv",header=TRUE)
# props3 <- read.csv("KModesCluster3Props.csv",header=TRUE)


# ======= Heat Map for Cluster 1 ==========#

# reformat dataframe of proportions for plotting
dataprops1 <- as.data.frame(cbind(c(props1[,1],props1[,2],props1[,3]),
                    rep(c("Yea","Nea","?"),each=17),
                    rep(paste("Key Vote",seq(1,17,1)),times=3)),stringsAsFactors=F)

colnames(dataprops1) <- c("Proportion","Outcome","Vote")

dataprops1$Proportion <- as.numeric(dataprops1$Proportion)

dataprops1$Vote <- factor(dataprops1$Vote,levels=paste("Key Vote",seq(17,1,-1)),
                             labels=rev(c("Party","Infants","Water","Budget","Physician",
                                      "El Salvador","Religion","Satellite",
                                      "Nicaragua","Missile","Immigration","Synfuels",
                                      "Education","Superfund","Crime",
                                      "Duty-Free","Exports")))

dataprops1$Outcome <- factor(dataprops1$Outcome,levels=c("Yea","Nea","?"),
                     labels=c("Yea","Nea","?"))

pdf("Cluster1Proportions.pdf",height=8.5,width=11)
theme_set(theme_grey(base_size=20))
ggplot(dataprops1, aes(Outcome, Vote)) +
  geom_tile(aes(fill = Proportion),colour = "white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  ggtitle(paste("Cluster 1 Size =",clustsize[1])) + xlab("") + ylab("") +
  geom_text(data=dataprops1, mapping=aes(x=Outcome, y=Vote, label=round(Proportion,3)),size=6)
dev.off()


# ======= Heat Map for Cluster 2 ==========#

# reformat dataframe of proportions for plotting
dataprops2 <- as.data.frame(cbind(c(props2[,1],props2[,2],props2[,3]),
                                  rep(c("Yea","Nea","?"),each=17),
                                  rep(paste("Key Vote",seq(1,17,1)),times=3)),stringsAsFactors=F)

colnames(dataprops2) <- c("Proportion","Outcome","Vote")

dataprops2$Proportion <- as.numeric(dataprops2$Proportion)

dataprops2$Vote <- factor(dataprops2$Vote,levels=paste("Key Vote",seq(17,1,-1)),
                          labels=rev(c("Party","Infants","Water","Budget","Physician",
                                       "El Salvador","Religion","Satellite",
                                       "Nicaragua","Missile","Immigration","Synfuels",
                                       "Education","Superfund","Crime",
                                       "Duty-Free","Exports")))

dataprops2$Outcome <- factor(dataprops2$Outcome,levels=c("Yea","Nea","?"),
                             labels=c("Yea","Nea","?"))

pdf("Cluster2Proportions.pdf",height=8.5,width=11)
theme_set(theme_grey(base_size=20))
ggplot(dataprops2, aes(Outcome, Vote)) +
  geom_tile(aes(fill = Proportion),colour = "white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  ggtitle(paste("Cluster 2 Size =",clustsize[2])) + xlab("") + ylab("") +
  geom_text(data=dataprops2, mapping=aes(x=Outcome, y=Vote, label=round(Proportion,3)),size=6)
dev.off()

# ======= Heat Map for Cluster 3 ==========#

# reformat dataframe of proportions for plotting
dataprops3 <- as.data.frame(cbind(c(props3[,1],props3[,2],props3[,3]),
                                  rep(c("Yea","Nea","?"),each=17),
                                  rep(paste("Key Vote",seq(1,17,1)),times=3)),stringsAsFactors=F)

colnames(dataprops3) <- c("Proportion","Outcome","Vote")

dataprops3$Proportion <- as.numeric(dataprops3$Proportion)

dataprops3$Vote <- factor(dataprops3$Vote,levels=paste("Key Vote",seq(17,1,-1)),
                          labels=rev(c("Party","Infants","Water","Budget","Physician",
                                       "El Salvador","Religion","Satellite",
                                       "Nicaragua","Missile","Immigration","Synfuels",
                                       "Education","Superfund","Crime",
                                       "Duty-Free","Exports")))

dataprops3$Outcome <- factor(dataprops3$Outcome,levels=c("Yea","Nea","?"),
                             labels=c("Yea","Nea","?"))

pdf("Cluster3Proportions.pdf",height=8.5,width=11)
theme_set(theme_grey(base_size=20))
ggplot(dataprops3, aes(Outcome, Vote)) +
  geom_tile(aes(fill = Proportion),colour = "white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  ggtitle(paste("Cluster 3 Size =",clustsize[3])) + xlab("") + ylab("") +
  geom_text(data=dataprops3, mapping=aes(x=Outcome, y=Vote, label=round(Proportion,3)),size=6)
dev.off()
