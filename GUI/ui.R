library(shiny)
library(shinydashboard)

###############################
# UI Code
###############################

sidebar <- dashboardSidebar(
             sidebarMenu(
               menuItem('Dashboard',tabName='dash',icon=icon('dashboard')),
               menuItem('Literature',tabName='lit_review',icon=icon('book')),
               menuItem('Data Simulation',tabName = 'data',icon=icon('table')),
               menuItem('Models',tabName = 'model',icon=icon('random')),
               menuItem('Visualizations',tabName = 'plots',icon=icon('area-chart'))
             )
           )

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dash",
            titlePanel("Capstone Project"),
            HTML('<center><img src="smu.jpg" width=500 height=300></center>')
    ),
    tabItem(tabName = "lit_review",
            h2("Literature Review")
    ),
    tabItem(tabName = "data",
            titlePanel("Select or Simulate Dataset"),
            fluidPage(
              sidebarLayout(
                # Load Data
                sidebarPanel(width=6,
                             fileInput("file1", "Choose CSV File",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain", ".csv")), 
                             tags$hr(),
                             
                             # Add checkbox if header exists
                             checkboxInput("header", "Dataset has Header", TRUE),
                             
                             # Type of file Separator
                             radioButtons("sep", "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ",")
                              ),
    
                             # Main view to display results
                             mainPanel(tableOutput('contents'))
                )
            )
        )
   )
)
