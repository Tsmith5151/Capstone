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
  
  
  results_matrix = matrix(c(0,0,0,0,0,0), nrow=10, ncol=9, byrow = TRUE)
  dimnames(results_matrix) = list( 
    c("St. dev 1", "", "", "", "", "", "", "", "", ""),         # row names 
    c("nVar","TPR", "FPR", "Recall", "Precision", "F1", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
  # Iterate over Variance 
  i <- 0
  for (nvar in seq(from=0.50,to=5.0,by=0.50)){
    i <- i+1
    # Initialize List and Append Results
    tpr <- c()
    fpr <- c()
    precision <- c()
    recall <- c()
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
      
      # TRP
      browser()
      tp <- performance(pred, measure = "tpr")
      tp_df = as.data.frame(tp@y.values[[1]])
      tpr <- c(tpr, tp)
      
      # FPR
      #fp <- performance(pred, measure = "fpr")
      #fpr <- c(fpr, fp)
      
      # Precision/Recall
      #p <- performance(pred,measure='prec')
      #precision <- c(precision, p)
      
      # Recall
      #rc <- performance(pred, measure='rec')
      #recall <- c(recall, rc)
      
      # F1 Score
      f <- performance(pred,measure="f")
      x = as.data.frame(f@y.values[[1]])
      f <- max(tail(x,-1))
      f1 <- c(f1,f)

      #Accuracy
      a <- performance(pred, measure = "acc")
      x = as.data.frame(a@y.values[[1]])
      a <- max(x)
      acc <- c(acc,a)

      # AUC
      a <- performance(pred,measure="auc")
      a <- a@y.values[[1]]
      auc <- c(auc, a)
      
      # Cost
      c <- performance(pred,measure="cost")
      c <- min(c@y.values[[1]])
      cost <- c(cost, c)
      
    }
    results_matrix <- populateMatrix(results_matrix, i, nvar, tpr, fpr, precision, recall, f1, acc, auc, cost) 
    
  }
  #df <- as.data.frame((sim_results))
  return(results_matrix)
  }
  
populateMatrix <- function (results_matrix, i, nvar, tpr, fpr, precision, recall, f1, acc, auc, cost){
  results_matrix[i,1] <- nvar
  
  results_matrix[i,2] <- mean(tpr)

  results_matrix[i,3] <- mean(fpr)
  
  results_matrix[i,4] <-mean(recall)
  
  results_matrix[i,5] <-mean(precision)
  
  results_matrix[i,6] <- mean(f1)
  
  results_matrix[i,7] <- mean(acc)

  results_matrix[i,8] <- mean(auc)
  
  results_matrix[i,9] <- mean(cost)
  
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
