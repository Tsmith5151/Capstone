library(shiny)
library(shinydashboard)
source('ui.R')

ui <- dashboardPage(
  skin='red',
  dashboardHeader(title = "SMU Data Science"),
  sidebar,
  body
)

server <- function(input, output) {
        
    output$contents <- renderTable({
      
      req(input$file1)
      
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
     return(df)
    })
    
}


shinyApp(ui, server)