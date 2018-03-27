library(simstudy)
library(ggplot2)
library(GGally)
library(randomForest)
library(ROCR)
library(caTools)
set.seed(1)


linear_eq <- function(n_ev,weights,y_int){
  
  # Explanatory Variables
  ev <- c()
  for (i in (1:n_ev)){
    ev[[i]] <- (paste0(weights,'*EV',i,sample(c('-','+'),1)))
  }
  
  # Build Equation
  x <- (paste(unlist(ev), collapse=' '))
  formula <- paste0(y_int,' + ',x)
  return(substring(formula,1,nchar(formula)-1))
  
}

sim_data <- function(n_obs,n_noise,ndist,nvar,n_ev,weights,y_int){
  
  data <- c()
  # Noise Variables
  for (i in 1:n_noise){
    data<- defData(data,varname=paste0('N',i), dist=ndist, formula = "0", variance = nvar, link = "identity")
  }
  
  # Explanatory Variables
  for (i in 1:n_ev){
    data<- defData(data,varname=paste0('EV',i), dist="normal", formula = "0", variance = .1, link = "identity")
  }

  data <- defDataAdd(data,varname="y", dist="binary", formula=linear_eq(n_ev,weights,y_int), link = "logit")
  
  # Build Simulated Data 
  data <- as.data.frame.matrix(genData(n_obs,data))
  
  return(data[,2:(ncol(data))])
}


plot_data <- function(data){
  
  # Parameters:
  # ---------------
  # data: input simulated dataframe
  
  # Returns:
  # ---------------
  # Scatter Matrix of Simulated Dataset
  
  p <- ggpairs(data,lower=list(continuous=wrap("smooth", colour="navy")),
               diag=list(continuous=wrap("barDiag", fill="darkcyan")))
  
  plot <- p + labs(title= paste("Simulated Dataset")) + theme(plot.title = element_text(size=14,face="bold")) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}

runLogistic <- function(data) {
  model = glm(y ~., family = "binomial", data = data)
  return(model)
}

server <- function(input, output) {
  
  # Return the requested dataset ----
  simdata <- reactive({
    sim_data(input$nrows,input$noise,input$ndist,input$nvar,input$ev,input$weights,input$yint)
  })
  
  equation <- reactive({
    linear_eq(input$ev,input$weights,input$yint)
  })

  # Show Table
  output$table <- renderTable({
    head(simdata(), n = 10)
  })
  
  # Print Equation
  output$equation <- renderPrint({
    paste0("y = ",equation())
  })
    
  # Show Plot
  output$plot1 <- renderPlot({
    # data<- sim_data(input$nrows,input$noise,input$ndist,input$nvar,input$ev,input$weights,input$yint)
    # data <-data[,2:(ncol(data)-1)]
    # cols <- sapply(data,is.integer)
    # data <- data[,!cols]
    # ggpairs(data,lower=list(continuous=wrap("smooth", colour="navy")),diag=list(continuous=wrap("barDiag", fill="darkcyan")))
  })
  
  # Logistic regression
  lr <- reactive({
    data <- sim_data(input$nrows,input$noise,input$ndist,input$nvar,input$ev,input$weights,input$yint)
    runLogistic(data)
  })
  
  # Show logistic regression
  output$lr_summary <- renderPrint({
    summary(lr())
  })
  
  # Create density and trace plots of model.
  output$lr_plot <- renderPlot({
    plot(lr())
  })
  
  # Perform ROC analysis.
  lr_roc <- reactive({
    predict <- predict(lr(), type = 'response')
    ROCRpred <- prediction(predict, sim_data(input$nrows,input$noise,input$ndist,input$nvar,input$ev,input$weights,input$yint)$y)
    ROCRperf <- performance(ROCRpred, 'tpr','fpr')
  })
  
  # Create ROC plot.
  output$lr_roc_plot <- renderPlot({
    plot(lr_roc(), colorize = TRUE, text.adj = c(-0.2,1.7))
  })
}