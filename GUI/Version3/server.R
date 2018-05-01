library(simstudy)
library(ggplot2)
library(GGally)
library(randomForest)
library(ROCR)
library(caTools)
#library(caret)
set.seed(1)


linear_eq <- function(n_ev,weights,y_int){
  
  # Explanatory Variables
  ev <- c()
  if(weights == 1) {
    for (i in (1:n_ev)){
      ev[[i]] <- (paste0('0.5*EV',i,sample(c('-','+'),1)))
    }
  } else if(weights == 2) {
    for (i in (1:n_ev)){
      if(i <= n_ev / 2) {
        ev[[i]] <- (paste0('0.3*EV',i,sample(c('-','+'),1)))
      }
      else {
        ev[[i]] <- (paste0('0.7*EV',i,sample(c('-','+'),1)))
      }
    }
  }else {
    for (i in (1:n_ev)){
      if(i <= n_ev / 3) {
        ev[[i]] <- (paste0('0.2*EV',i,sample(c('-','+'),1)))
      }
      else if(i <= 2 * n_ev / 3){
        ev[[i]] <- (paste0('0.5*EV',i,sample(c('-','+'),1)))
      }
      else {
        ev[[i]] <- (paste0('0.8*EV',i,sample(c('-','+'),1)))
      }
    }
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
    data<- defData(data,varname=paste0('EV',i), dist="normal", formula = "0", variance = nvar, link = "identity")
  }
  
  data <- defDataAdd(data,varname="y", dist="binary", formula=linear_eq(n_ev,weights,y_int), link = "logit")
  
  # Build Simulated Data 
  data <- as.data.frame.matrix(genData(n_obs,data))
  
  return(data[,2:(ncol(data))])
}

# Split Training/Testing Sets
split_data <- function(data, r_split) {
  set.seed(123)
  sample = sample.split(data$y,SplitRatio = r_split)
  TRAIN <<- subset(data,sample ==TRUE)
  TEST <<- subset(data, sample==FALSE)
}

# Logistic Regression Model
lr_simulation <- function(n_sim,split,nrows,noise,ndist,nvar,ev,weights,yint,varselect){
  
  sim_results <- c()
  results_matrix = matrix(c(0,0,0,0,0,0), nrow=10, ncol=9, byrow = TRUE)
  dimnames(results_matrix) = list( 
    c("St. dev 1", "", "", "", "", "", "", "", "", ""),         # row names 
    c("nVar","True Positive Rate", "False Positive Rate", "Recall", "Precision", "F Measure", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
  # Iterate over Variance 
  i <- 0
  for (nvar in seq(from=0.50,to=5.0,by=0.50)){
    i <- i+1
    # Initialize List and Append Results
    tpr_fpr <- c()
    precision_recall <- c()
    f1 <- c()
    acc <- c()
    auc <- c()
    cost <- c()

    # Number of Simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ndist,nvar,ev,weights,yint)

      # Split Train/Testing
      split_data(data,split)
      
      # LR Model
      model.train = step(lm(y ~., family = "binomial", data = TRAIN), direction = varselect)
      
      # Prediction
      pred <- predict(model.train,TEST[, !names(TEST) %in% c("y")] ,type = 'response')
      pred<-prediction(pred, TEST$y)
      
      # TRP/FPR
      tpr_fpr <- c(tpr_fpr, performance(pred, measure = "tpr",x.measure = "fpr"))
      
      # Precision/Recall
      precision_recall <- c(precision_recall, performance(pred,measure='prec',x.measure='rec'))
      
      # F1 Score
      f1 <- c(f1,performance(pred,measure="f"))
      
      #Accuracy
      acc <- c(acc,performance(pred, measure = "acc"))
      
      # AUC
      auc <- c(auc, performance(pred,measure="auc"))
      
      # Cost
      cost <- c(cost, performance(pred,measure="cost"))
      
    }
    #print(tpr_fpr[[1]])
    results_matrix[i,1] <- nvar
    browser()
    tpr <- slot(tpr_fpr[[1]], "y.values")
    results_matrix[i,2] <- mean(tpr[[1]])
    #tpr_fpr[[3]]@y.values[[1]][21]
    
    fpr <- slot(tpr_fpr[[1]], "x.values")
    results_matrix[i,3] <- mean(fpr[[1]])
    
    recall <-slot(precision_recall[[1]], "x.values")
    results_matrix[i,4] <-mean(recall[[1]])
    
    precision <-slot(precision_recall[[1]], "y.values")
    results_matrix[i,5] <-mean(precision[[1]])
    
    fmeasure <- slot(f1[[1]], "y.values")
    results_matrix[i,6] <- mean(fmeasure[[1]])
    
    accuracy <- slot(acc[[1]], "y.values")
    results_matrix[i,7] <- mean(accuracy[[1]])
    
    areaundercurve <- slot(auc[[1]], "y.values")
    results_matrix[i,8] <- mean(areaundercurve[[1]])
    
    excost <- slot(cost[[1]], "y.values")
    results_matrix[i,9] <- mean(excost[[1]])
  }
  
  #df <- as.data.frame((sim_results))
  return(results_matrix)
  }
  


server <- function(input, output) {
  
  # Return the requested dataset ----
  simdata <- reactive({
    SIM_DATA <<- sim_data(input$nrows,input$noise,input$ndist,input$nvar,input$ev,input$weights,input$yint)
  })
  
  equation <- reactive({
    linear_eq(input$ev,input$weights,input$yint)
  })
  
  # Show Table
  output$table <- renderTable({
    head(simdata(), n = 30)
  })
  
  # Print Equation
  output$equation <- renderText({
    paste0("y = ",equation())
  })
  
  # Logistic Simulation
  lr <- reactive({
    withProgress(message = 'Simulating Data', value = 0,
                 run_model <- lr_simulation(input$n_sim,input$split,input$nrows,input$noise,input$ndist,nvar,input$ev,input$weights,
                                            input$yint,input$varselect)
    )
  })
  
  output$lr_title <- renderText({
    paste0( "Total Number of Simulations: ", input$n_sim)
  })
  
  # Print Equation
  output$lr_sim <- renderTable({
    LR <<- lr()
  })
  
  #output$lr_sim_chart <- renderPlot({
    #plot(LR[,'Variance'],LR[,'Accuracy'],xlab='Variance',ylab='Accuracy',main='Variance vs Hold out Accuracy',
         #col='blue',type='l',lwd=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  #})
}
