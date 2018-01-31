library(shiny)
library(shinydashboard)
source('ui.R')

ui <- dashboardPage(
  skin='red',
  dashboardHeader(title = "SMU Data Science"),
  sidebar,
  body
)


###############################
# Server Side
###############################

palette(c("cyan4", "coral2", "darkorchid4", "darkseagreen4",
          "navy", "darkgoldenrod1", "#A65628", "#F781BF", "#999999"))

server <- function(input, output) {
  
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), centers=input$clusters,iter.max=10)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),col = clusters()$cluster,pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}


shinyApp(ui, server)