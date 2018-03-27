# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # Title
  titlePanel("SMU Data Science"),
  h3("Simulation Study: Logistic Regression vs Random Forest"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar Inputs
    sidebarPanel(
      
      # Input: Number of Rows
      numericInput(inputId = "nrows",
                   label = "Number of Rows",
                   value = 100,min=0,max = 10000),
      
      br(),
      # Input: Noise Variables
      numericInput(inputId = "noise",
                   label = "Noise Variables",
                   value = 2,min=0,max = 100),
      
      # Distribution for Noise Variables
      selectInput("ndist", label ="Noise Variable Distribution", 
                  choices = list("Normal" = "normal", 
                                 "Gamma" = "gamma"), 
                  selected = "normal"),
      
      # Input: Noise Variables Variance
      numericInput(inputId = "nvar",
                   label = "Noise Varance",
                   value = 1,min=0,max = 10,step=0.50),
      
      br(),
      # Input: Explanatory Variables
      numericInput(inputId = "ev",
                   label = "Explanatory Variable",
                   value = 10, min=0, max = 100, step=1),
      
      # Input: Explanatory Variables Weights
      numericInput(inputId = "weights",
                   label = "Explanatory Variable Coefficients",
                   value = 0.50, min=0, max = 1.0,step=0.10),
      br(),
      
      br(),
      # Input: y-intercept
      numericInput(inputId = "yint",
                   label = "Y-Intercept",
                   value = 0.10, min=0,max = 1.0, step = 0.10)
    ),
    
    # Main Panel
    mainPanel(
      
      # Outputs
      tabsetPanel(
        tabPanel("Simulated Data", 
                  fluidRow(
                    column(12,
                    h5(tableOutput("equation"))
                    )
                  ),
                 br(),
                  fluidRow(column(12,tableOutput("table"))),
                 br(),
                 fluidRow(column(12,plotOutput("plot1")))
                 ), 
        tabPanel("Logistic Regression", 
                 fluidRow(column(12,tableOutput("lr")))),
        tabPanel("Random Forest", tableOutput("rf"))
    )
  )
)
)
