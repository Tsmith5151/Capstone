# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  theme = "style.css",
  # Title
  img(src = "logo.png", width = "270px", height = "25px"),
  h3("Simulation Study: Logistic Regression vs Random Forest"),
  
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
      # Drop down 1,2,3 
      # If 1 then all get the same weight
      # If 2 then half get one weight, other half get another weight
      # If 3 then ...
      selectInput(inputId = "weights",
                   label = "Explanatory Variable Coefficients",
                  choices = list("Balanced" = "1", 
                                 "Half large, half small" = "2",
                                 "Small, medium, and large" = "3"), 
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
      
      # LR - stepwise forward/ backward... AIC, 
      
      br(),      
      h4("Random Forest Parameters"),      
      sliderInput(inputId = "ntree",                   
                  label = "Number of Trees",                   
                  value = 100, min=50,max = 550, step = 1)
    
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
                 h5("TODO: get iterations working"),
                 h4("Model Steps - 10 iterations"),
                 fluidRow(column(12,verbatimTextOutput("lr_step"))),
                 h4("Average Model Summary"),
                 fluidRow(column(12,verbatimTextOutput("lr_summary"))),
                 h4("Confusion Matrix"),
                 fluidRow(column(12,verbatimTextOutput("lr_predict"))),
                 h4("Model Plot"),
                 fluidRow(column(12,plotOutput("lr_plot"))),
                 h4("ROC Curve"),
                 fluidRow(column(12,plotOutput("lr_roc_plot")))
                 ),
        tabPanel("Random Forest",
                 h5("TODO: train + test to make it work?"),
                 h4("Random Forest Fit"),
                 fluidRow(column(12, verbatimTextOutput("rf_split"))),
                 fluidRow(column(12,verbatimTextOutput("rf_fit"))),
                 h4("Feature Importance"),
                 fluidRow(column(12,verbatimTextOutput("rf_importance"))),
                 tableOutput("rf"))
    )
  )
)
)
