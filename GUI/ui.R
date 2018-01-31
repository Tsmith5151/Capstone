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
            h2("Capstone Project 2018")
    ),
    tabItem(tabName = "lit_review",
            h2("Literature Review")
    ),
    tabItem(tabName = "data",
            h2("Data Simulation")
    ),
    tabItem(tabName = "model",
            h2("K-Means Clustering"),
            fluidRow(
              box(
                  status='primary',
                  plotOutput('plot1',height=250)),
              box(
                title='Inputs',
                status = 'warning',
                solidHeader = TRUE,
                sidebarPanel(
                  selectInput(inputId = 'xcol', 'X-Variable', names(iris)),
                  selectInput(inputId = 'ycol', 'Y-Variable', names(iris),
                              selected = names(iris)[[2]]),
                  numericInput(inputId = 'clusters', 'Total Clusters', 3, min = 1, max = 9)
                )
              )
            )
    ),
    tabItem(tabName = "plots",
            h2("Model Evaluation on Training/Testing Sets")
    )
  )
)
