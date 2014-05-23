# Identify clusters in congressional voting records
# Load PR3q First!!
head(PR3q)
# Dems = 1, Reps =2
# ? = 1, no = 2, yes =3
# Transform data to numeric
table(PR3q[,1])
for(i in 1:17){
  PR3q[,i]=as.numeric(PR3q[,i])
}
# K modes
# Scree plot: 
cl1 <- kmodes(PR3q, 1)
cl2 <- kmodes(PR3q, 2)
cl3 <- kmodes(PR3q, 3)
cl4 <- kmodes(PR3q, 4)
cl5 <- kmodes(PR3q, 5)
cl6 <- kmodes(PR3q, 6)
x=c(sum(cl1$with),sum(cl2$with),sum(cl3$with),sum(cl4$with),sum(cl5$with),sum(cl6$with))
plot(1:6,x) 

## and visualize with some jitter:
#plot(jitter(PR3q[,1]),jitter(PR3q[,2]), col = cl$cluster,pch=cl$cluster)
#points(cl$modes, col = 1:17, pch = 8)

# Make dissimilaries matrix (SLOW), saved as file dismat
mat=matrix(rep(0,435*435),nrow=435,ncol=435)
count=0
for(i in 1:435){
  print(i)
  for(j in 1:435){
    count = 0
    for(k in 1:17){
    count = count+ifelse(PR3q[i,k]==PR3q[j,k],0,1)
    }
    mat[i,j] = count
}}

# Multi dimensional scaling
cmd=cmdscale(mat,k=2)
qplot(cmd[,1],cmd[,2],colour=as.factor(cl3$cluster))

shinyServer(function(input, output) {
  #sel <- reactive(quote({input$checkGroup}),quoted=TRUE)
  cl <- reactive(kmodes(PR3q[,1:2], input$slider2))
  output$plt1 = renderPlot(plot(jitter(PR3q[,input$slider[1]]),jitter(PR3q[,input$slider[2]]), col = cl()$cluster,
                                xlab=paste("Question",input$slider[1]),
                                ylab=paste("Question",input$slider[2])))
  
  })



