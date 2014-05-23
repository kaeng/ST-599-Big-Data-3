shinyUI(fluidPage(
  titlePanel("Voting"),
  sidebarLayout(
    sidebarPanel(
      p("Create K-Modes comparison plots"),
      p("How many modes?"),
      sliderInput("slider2", label = "",
                  min = 1, max = 5, value = 2),
      p("Which pair of variables?"),
      sliderInput("slider", "",
                  min = 1, max = 17, value = c(1, 2)),
      br(),
      p("q1:Dem(1) or Rep(2)")
    ),
    mainPanel(
      textOutput("txt1"),
      textOutput("txt2"),
      plotOutput("plt1")
    )
  
  
)))
