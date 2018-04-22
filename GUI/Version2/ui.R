# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  theme = "style.css",
  # Title
  img(src = "logo.png", width = "270px", height = "25px"),
  h3("Comparison of Classification Performance of Random Forest and Logistic Regression"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar Inputs
    sidebarPanel(
      h4("Simulate your data"),
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
                   label = "Noise Variance",
                   value = 1,min=0,max = 10,step=0.50),
      
      br(),
      # Input: Explanatory Variables
      numericInput(inputId = "ev",
                   label = "Explanatory Variable",
                   value = 10, min=0, max = 100, step=1),
      
      # Input: Explanatory Variables Weights
      selectInput(inputId = "weights",
                  label = "Explanatory Variable Coefficients",
                  choices = list("Uniform: 0.50" = "1", 
                                 "Small/Large: 0.30, 0.70" = "2",
                                 "Small/Medium/Large: 0.20, 0.50, 0.80" = "3"), 
                  selected = "1"),
      br(),
      # Input: y-intercept
      numericInput(inputId = "yint",
                   label = "Y-Intercept",
                   value = 0.10, min=0,max = 1.0, step = 0.10),
      
      br(),      
      h4("Model Input Parameters"),            
      numericInput(inputId = "split",
                   label = "Train/Test Split",
                   value = 0.70, min=0,max = 1.0, step = 0.05),        
      
      br(), 
      h4("Variable Selection Model"),            
      selectInput(inputId = "varselect",
                  label = "Stepwise Selection Model",
                  choices = list("backward" = "backward", 
                                 "forward" = "forward",
                                 "forward & backward" = "both"), 
                  selected = "forward"),      
      br(), 
      h4("Random Forest Parameters"),      
      sliderInput(inputId = "ntree",                   
                  label = "Number of Trees",                   
                  value = 100, min=50,max = 550, step = 1),
      
      br(),      
      h4("Simulation: Number of Iterations"),      
      sliderInput(inputId = "n_sim",                   
                  label = "Number of Simulations",                   
                  value = 10, min=1,max = 1000, step = 1)
    ),
    
    # Main Panel
    mainPanel(
      
      # Outputs
      tabsetPanel(
        tabPanel("Simulated Data", 
                 fluidRow(
                   column(12,
                          h4(tableOutput("equation"))
                   )
                 ),
                 br(),
                 fluidRow(column(12,tableOutput("table")))
        ), 
        tabPanel("Logistic Regression Simulation", 
                 fluidRow(column(12,verbatimTextOutput("lr_sim")))
        ),
        tabPanel("Random Forest Simulation",
                 h4("Random Forest Out of Bag Error")
        )
      )
    )
  )
)
