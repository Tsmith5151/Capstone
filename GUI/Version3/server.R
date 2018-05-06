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

# Random Forest
do_randomforest <- function(train,test,n_tree){
  
  # Fit Model
  model.train <- randomForest(as.factor(y)~.,train,ntree=n_tree)
  
  # Prediction
  pred<-predict(model.train, test[, !names(test) %in% c("y")], type = 'response')
  pred <- prediction(as.numeric(pred), as.numeric(test$y))
  return(pred)
}

# Logistic Regression
do_logisticregression <- function(train,test,varselect){
  
  # Fit Model
  model.train = step(lm(y ~., family = "binomial", data = train), direction = varselect)
  
  # Prediction
  pred <- predict(model.train, test[, !names(test) %in% c("y")], type = 'response')
  pred <- prediction(pred, test$y)
  return(pred)
}


# Logistic Regression and Random Forest Simulation
simulation_lr_rf <- function(n_sim,split,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
  lr_matrix = matrix(c(0,0,0,0,0,0), nrow=10, ncol=9, byrow = TRUE)
  dimnames(lr_matrix) = list( 
    c("St. dev 1", "", "", "", "", "", "", "", "", ""),         # row names 
    c("nVar","TPR", "FPR", "Recall", "Precision", "F1", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
  rf_matrix = matrix(c(0,0,0,0,0,0), nrow=10, ncol=9, byrow = TRUE)
  dimnames(rf_matrix) = list( 
    c("St. dev 1", "", "", "", "", "", "", "", "", ""),         # row names 
    c("nVar","TPR", "FPR", "Recall", "Precision", "F1", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
  # Iterate over Variance 
  i <- 0
  for (nvar in seq(from=0.50,to=5.0,by=0.50)){
    i <- i+1
    # Initialize List and Append Results
    lr_tpr <- c()
    rf_tpr <- c()
    lr_fpr <- c()
    rf_fpr <- c()
    lr_precision <- c()
    rf_precision <- c()
    lr_recall <- c()
    rf_recall <- c()
    lr_f1 <- c()
    rf_f1 <- c()
    lr_acc <- c()
    rf_acc <- c()
    lr_auc <- c()
    rf_auc <- c()
    lr_cost <- c()
    rf_cost <- c()

    # Number of Simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ndist,nvar,ev,weights,yint)

      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # TRP
      tp <- performance(lr_pred, measure = "tpr")
      tp = mean(tp@y.values[[1]])
      lr_tpr <- c(lr_tpr, tp)
      
      tp <- performance(rf_pred, measure = "tpr")
      tp = tp@y.values[[1]][2]
      rf_tpr <- c(rf_tpr, tp)
      
      # FPR
      fp <- performance(lr_pred, measure = "fpr")
      fp = mean(fp@y.values[[1]])
      lr_fpr <- c(lr_fpr, fp)
      
      fp <- performance(rf_pred, measure = "fpr")
      fp = fp@y.values[[1]][2]
      rf_fpr <- c(rf_fpr, fp)
      
      # Precision/Recall
      p <- performance(lr_pred,measure='prec')
      x = p@y.values[[1]]
      x = tail(x,-1)
      p <- mean(x)
      lr_precision <- c(lr_precision, p)
      
      p <- performance(rf_pred,measure='prec')
      x = p@y.values[[1]]
      x = tail(x,-1)
      p <- mean(x)
      rf_precision <- c(rf_precision, p)
      
      # Recall
      rc <- performance(lr_pred, measure='rec')
      rc = mean(rc@y.values[[1]])
      lr_recall <- c(lr_recall, rc)
      
      rc <- performance(rf_pred, measure='rec')
      rc = rc@y.values[[1]][2]
      rf_recall <- c(rf_recall, rc)
      
      # F1 Score
      f <- performance(lr_pred,measure="f")
      x = as.data.frame(f@y.values[[1]])
      f <- max(tail(x,-1))
      lr_f1 <- c(lr_f1,f)
      
      f <- performance(rf_pred,measure="f")
      x = as.data.frame(f@y.values[[1]])
      f <- max(tail(x,-1))
      rf_f1 <- c(rf_f1,f)
      
      #Accuracy
      a <- performance(lr_pred, measure = "acc")
      x = as.data.frame(a@y.values[[1]])
      a <- max(x)
      lr_acc <- c(lr_acc,a)
      
      a <- performance(rf_pred, measure = "acc")
      a <- max(a@y.values[[1]])
      rf_acc <- c(rf_acc,a)

      # AUC
      a <- performance(lr_pred,measure="auc")
      a <- a@y.values[[1]]
      lr_auc <- c(lr_auc, a)
      
      a <- performance(rf_pred,measure="auc")
      a <- a@y.values[[1]]
      rf_auc <- c(rf_auc, a)
      
      # Cost
      c <- performance(lr_pred,measure="cost")
      c <- min(c@y.values[[1]])
      lr_cost <- c(lr_cost, c)
      
      c <- performance(rf_pred,measure="cost")
      c <- min(c@y.values[[1]])
      rf_cost <- c(rf_cost, c)
      
    }
    lr_matrix <- populateMatrix(lr_matrix, i, nvar, lr_tpr, lr_fpr, lr_precision, lr_recall, lr_f1, lr_acc, lr_auc, lr_cost) 
    rf_matrix <- populateMatrix(rf_matrix, i, nvar, rf_tpr, rf_fpr, rf_precision, rf_recall, rf_f1, rf_acc, rf_auc, rf_cost)
  }
  #df <- as.data.frame((sim_results))
  result=list(lr_matrix, rf_matrix) 
  return(result)
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
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_lr_rf(input$n_sim,
                                               input$split,
                                               input$nrows,
                                               input$noise,
                                               input$ndist,
                                               nvar,input$ev,
                                               input$weights,
                                               input$yint,
                                               input$varselect, # Logistic Regression
                                               input$ntree # Random Forest
                                               )
    )
  })
  
  output$lr_title <- renderText({
    paste0( "Total Number of Logistic Regression Simulations: ", input$n_sim)
  })
  
  output$rf_title <- renderText({
    paste0( "Total Number of Random Forest Simulations: ", input$n_sim)
  })
  
  # Print LR Matrix
  output$lr_sim <- renderTable({
    LR <<- lr()
    LR[1]
  })
  
  # Print RF Matrix
  output$rf_sim <- renderTable({
    LR[2]
  })
  
  #output$lr_sim_chart <- renderPlot({
    #plot(LR[,'Variance'],LR[,'Accuracy'],xlab='Variance',ylab='Accuracy',main='Variance vs Hold out Accuracy',
         #col='blue',type='l',lwd=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  #})
}
