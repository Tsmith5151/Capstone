# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # Title
  titlePanel("SMU Data Science"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar Inputs
    sidebarPanel(
      
      # Input: Number of Rows
      numericInput(inputId = "nrows",
                   label = "Number of Rows",
                   value = 1,min=0,max = 100),
      
      br(),
      # Input: Noise Variables
      numericInput(inputId = "noise",
                   label = "Noise Variables",
                   value = 1,min=0,max = 100),
      
      # Distribution for Noise Variables
      selectInput("ndist", label ="Noise Variable Distribution", 
                  choices = list("Normal" = "normal", 
                                 "Gamma" = "gamma"), 
                  selected = "normal"),
      
      # Input: Noise Variables Variance
      numericInput(inputId = "nvar",
                   label = "Noise Varance",
                   value = 1,min=0,max = 10),
      
      br(),
      # Input: Explanatory Variables
      numericInput(inputId = "ev",
                   label = "Explanatory Variable",
                   value = 1,min=0,max = 100)
    ),
    
    # Main Panel
    mainPanel(
      
      # Outputs
      tabsetPanel(
        tabPanel("Simulated Data", tableOutput("table")), 
        tabPanel("Summary", tableOutput("summary")), 
        tabPanel("Visualization", tableOutput("plot"))
    )
  )
)
)