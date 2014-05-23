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
#cl <- kmodes(PR3q, 4)
## and visualize with some jitter:
#plot(jitter(PR3q[,1]),jitter(PR3q[,2]), col = cl$cluster)
#points(cl$modes, col = 1:17, pch = 8)

shinyServer(function(input, output) {
  #sel <- reactive(quote({input$checkGroup}),quoted=TRUE)
  cl <- reactive(kmodes(PR3q[,1:2], input$slider2))
  output$plt1 = renderPlot(plot(jitter(PR3q[,input$slider[1]]),jitter(PR3q[,input$slider[2]]), col = cl()$cluster,
                                xlab=paste("Question",input$slider[1]),
                                ylab=paste("Question",input$slider[2])))
  
  })



