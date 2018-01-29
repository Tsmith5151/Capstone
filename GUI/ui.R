library(shiny)

palette(c("cyan4", "coral2", "darkorchid4", "darkseagreen4",
          "navy", "darkgoldenrod1", "#A65628", "#F781BF", "#999999"))

ui <- fluidPage(
  
  titlePanel('SMU Data Science'),
  
  sidebarPanel(
    selectInput(inputId = 'xcol', 'X-Variable', names(iris)),
    selectInput(inputId = 'ycol', 'Y-Variable', names(iris),
                selected = names(iris)[[2]]),
    numericInput(inputId = 'clusters', 'Total Clusters', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
