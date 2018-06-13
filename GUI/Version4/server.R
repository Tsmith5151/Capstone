library(simstudy)
library(ggplot2)
library(GGally)
library(randomForest)
library(ROCR)
library(caTools)
library(corrplot)
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

sim_data <- function(n_obs,n_noise,cat,ndist,nvar,n_ev,weights,y_int){
  
  data <- c()
  
  # Noise Variables
  for (i in 1:n_noise){
    data<- defData(data,varname=paste0('N',i), dist=ndist, formula = "0", variance = nvar, link = "identity")
  }
  
  # Explanatory Variables
  for (i in 1:n_ev){
    data<- defData(data,varname=paste0('EV',i), dist="normal", formula = "0", variance = nvar, link = "identity")
  }
  
  #Categorical Variables
  if(cat != 0){
    for (i in 1:cat){
    data<- defData(data,varname=paste0('Cat',i), dist="categorical", formula = "0.3;0.2;0.5")
    }
  }
  
  # Response Variable
  data <- defDataAdd(data,varname="y", dist="binary", formula=linear_eq(n_ev,weights,y_int), link = "logit")
  
  # Build Simulated Data 
  data <- as.data.frame.matrix(genData(n_obs,data))
  
  # Convert Categorical Variables to Factors
  cols =grep('Cat', names(data), value=TRUE) 
  data[cols] <- lapply(data[cols], factor)
  
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


# Run Logistic Regression and Random Forest Simulations sweeping variance of variables
simulation_nvar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
  
  lr_matrix = matrix(c(0,0,0,0,0,0), nrow=10, ncol=9, byrow = TRUE)
  dimnames(lr_matrix) = list( 
    c("", "", "", "", "", "", "", "", "", ""),         # row names 
    c("nVar","TPR", "TNR", "Recall", "Precision", "F1", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
  rf_matrix = matrix(c(0,0,0,0,0,0), nrow=10, ncol=9, byrow = TRUE)
  dimnames(rf_matrix) = list( 
    c("", "", "", "", "", "", "", "", "", ""),         # row names 
    c("nVar","TPR", "TNR", "Recall", "Precision", "F1", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
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
      data<-sim_data(nrows,noise,ncat,ndist,nvar,ev,weights,yint)
      
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # TRP
      tp <- performance(lr_pred, measure = "tpr")
      tp = tp@y.values[[1]][23]
      lr_tpr <- c(lr_tpr, tp)
      
      tp <- performance(rf_pred, measure = "tpr")
      tp = tp@y.values[[1]][2]
      rf_tpr <- c(rf_tpr, tp)
      
      # FPR
      fp <- performance(lr_pred, measure = "tnr")
      fp = fp@y.values[[1]][23]
      lr_fpr <- c(lr_fpr, fp)
      
      fp <- performance(rf_pred, measure = "tnr")
      fp = fp@y.values[[1]][2]
      rf_fpr <- c(rf_fpr, fp)
      
      # Precision/Recall
      p <- performance(lr_pred,measure='prec')
      p = p@y.values[[1]][23]
      lr_precision <- c(lr_precision, p)
      
      p <- performance(rf_pred,measure='prec')
      p = p@y.values[[1]][2]
      rf_precision <- c(rf_precision, p)
      
      # Recall
      rc <- performance(lr_pred, measure='rec')
      rc = rc@y.values[[1]][23]
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
      a = a@y.values[[1]][23]
      lr_acc <- c(lr_acc,a)
      
      a <- performance(rf_pred, measure = "acc")
      a = a@y.values[[1]][2]
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
    
    # Table
    lr_matrix <- populateMatrix(lr_matrix, i, nvar, lr_tpr, lr_fpr, lr_precision, lr_recall, lr_f1, lr_acc, lr_auc, lr_cost) 
    rf_matrix <- populateMatrix(rf_matrix, i, nvar, rf_tpr, rf_fpr, rf_precision, rf_recall, rf_f1, rf_acc, rf_auc, rf_cost)
    saveAUCScores(lr_auc,rf_auc, nvar)
  }
  #df <- as.data.frame((sim_results))
  result=list(lr_matrix, rf_matrix) 
  return(result)
}

saveAUCScores <- function(lr, rf, nvar) {
  lr <- as.data.frame(lr)
  colnames(lr) <- c("score")
  lr$alg <- "Logistic Regression"
  lr$nvar <- nvar
  rf <- as.data.frame(rf)
  colnames(rf) <- c("score")
  rf$alg <- "Random Forest"
  rf$nvar <- nvar
  temp <- rbind(lr, rf)
  if (exists("AUC")) {
     AUC <<- rbind(AUC, temp)
  }
   else {
    AUC <<- temp
   }
  AUC[,'nvar'] <<- as.factor(AUC[,'nvar'])
}

saveAUCScores2 <- function(lr, rf, noise) {
  lr <- as.data.frame(lr)
  colnames(lr) <- c("score")
  lr$alg <- "Logistic Regression"
  lr$noise <- noise
  rf <- as.data.frame(rf)
  colnames(rf) <- c("score")
  rf$alg <- "Random Forest"
  rf$noise <- noise
  temp <- rbind(lr, rf)
  if (exists("AUC")) {
    AUC2 <<- rbind(AUC2, temp)
  }
  else {
    AUC2 <<- temp
  }
  AUC2[,'noise'] <<- as.factor(AUC2[,'noise'])
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

get_case1_plots <- function(lr,rf,xvar,yvar){
  
  # Logistic Regression
  plot(lr[,xvar],lr[,yvar],xlab=xvar,ylab=yvar,main=paste0('Simulation Results: ',xvar,' vs ',yvar),
       col='blue',type='b',pch=19,lwd=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,ylim=c(0.50,1))
  
  # Random Forest
  lines(rf[,xvar],rf[,yvar],col='red',type='b',pch=18,lwd=2)
  
  # Legend
  legend("bottomright", legend=c("RF", "LR"),
         col=c("red", "blue"), lty=1:2, cex=0.8,text.font=4,box.lty=0)
}

get_case1_boxplots <- function() {
  ggplot(AUC, aes(x=nvar, y=score, fill=alg)) + geom_boxplot() + ggtitle('Logistic Regression vs Random Forest')
}

get_casen_boxplots <- function() {
  ggplot(AUC2, aes(x=noise, y=score, fill=alg)) + geom_boxplot() + ggtitle('Logistic Regression vs Random Forest')
}

lr_rf_varselection <- function(n_sim,split,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
  
  # Regenerate Data
  for (n in 1:n_sim){
    
    # Simulate Data
    data<-sim_data(nrows,noise,ndist,0.50,ev,weights,yint)
  
    # Split Train/Testing
    split_data(data,split)
    
    # Random Forest
    rf <- randomForest(as.factor(y)~.,TRAIN,ntree=ntrees)
    rf_importance <- importance(rf,type=2) 
    
    # Logistic Regression
    lr_importance = step(lm(y ~., family = "binomial", data = TRAIN), direction = varselect)
    
    # ToDo: Setup Summary Table of variable importance
  }
}

# Run Logistic Regression and Random Forest Simulations sweeping number of noise variables
simulation_num_nvar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
  
  lr_matrix = matrix(c(0,0,0,0,0,0), nrow=4, ncol=9, byrow = TRUE)
  dimnames(lr_matrix) = list( 
    c("", "", "", ""),         # row names 
    c("Num Noise","TPR", "TNR", "Recall", "Precision", "F1", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
  rf_matrix = matrix(c(0,0,0,0,0,0), nrow=4, ncol=9, byrow = TRUE)
  dimnames(rf_matrix) = list( 
    c("", "", "", ""),         # row names 
    c("Num Noise","TPR", "TNR", "Recall", "Precision", "F1", "Accuracy", "AUC", "Explicit Cost")) # column names 
  
  # Iterate over number of noise variables 
  i <- 0
  for (noise in c(1,5,10,20)){
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
      data<-sim_data(nrows,noise,ncat,ndist,nvar,ev,weights,yint)
      
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # TRP
      tp <- performance(lr_pred, measure = "tpr")
      tp = tp@y.values[[1]][23]
      lr_tpr <- c(lr_tpr, tp)
      
      tp <- performance(rf_pred, measure = "tpr")
      tp = tp@y.values[[1]][2]
      rf_tpr <- c(rf_tpr, tp)
      
      # FPR
      fp <- performance(lr_pred, measure = "tnr")
      fp = fp@y.values[[1]][23]
      lr_fpr <- c(lr_fpr, fp)
      
      fp <- performance(rf_pred, measure = "tnr")
      fp = fp@y.values[[1]][2]
      rf_fpr <- c(rf_fpr, fp)
      
      # Precision/Recall
      p <- performance(lr_pred,measure='prec')
      p = p@y.values[[1]][23]
      lr_precision <- c(lr_precision, p)
      
      p <- performance(rf_pred,measure='prec')
      p = p@y.values[[1]][2]
      rf_precision <- c(rf_precision, p)
      
      # Recall
      rc <- performance(lr_pred, measure='rec')
      rc = rc@y.values[[1]][23]
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
      a = a@y.values[[1]][23]
      lr_acc <- c(lr_acc,a)
      
      a <- performance(rf_pred, measure = "acc")
      a = a@y.values[[1]][2]
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
    
    # Table
    lr_matrix <- populateMatrix(lr_matrix, i, noise, lr_tpr, lr_fpr, lr_precision, lr_recall, lr_f1, lr_acc, lr_auc, lr_cost) 
    rf_matrix <- populateMatrix(rf_matrix, i, noise, rf_tpr, rf_fpr, rf_precision, rf_recall, rf_f1, rf_acc, rf_auc, rf_cost)
    saveAUCScores2(lr_auc,rf_auc, noise)
  }
  #df <- as.data.frame((sim_results))
  result=list(lr_matrix, rf_matrix) 
  return(result)
}

################################################
server <- function(input, output) {
  
  # Return the requested dataset ----
  simdata <- reactive({
    SIM_DATA <<- sim_data(input$nrows,input$noise,input$cat,input$ndist,input$nvar,input$ev,input$weights,input$yint)
  })
  
  equation <- reactive({
    linear_eq(input$ev,input$weights,input$yint)
  })
  
  # Show Table
  output$table <- renderTable({
    head(simdata(), n = 10)
  })
  
  output$cplot <- renderPlot({
    corrplot(cor(simdata()[,1:length(names(simdata()))-1]), method="shade",type='lower',tl.col = "black", tl.srt = 45)
  })
  
  # Print Equation
  output$equation <- renderText({
    paste0("y = ",equation())
  })
  
  # Logistic and Random forest simulation
  lr_rf_nvar <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_nvar(input$n_sim,
                                               input$split,
                                               input$cat,
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
  
  # Titles
  output$lr_title_nvar <- renderText({
    paste0( "Total Number of Logistic Regression Simulations: ", input$n_sim)
  })
  
  output$rf_title_nvar <- renderText({
    paste0( "Total Number of Random Forest Simulations: ", input$n_sim)
  })
  
  # Print LR Matrix
  output$lr_sim_nvar <- renderTable({
    LR_RF <<- lr_rf_nvar()
    LR_RF[1]
  })
  
  # Print RF Matrix
  output$rf_sim_nvar <- renderTable({
    LR_RF[2]
  })
  
  # CASE1 Plots
  output$case1_chart1 <- renderPlot({
    LR1 <<- as.data.frame(LR_RF[1])
    RF1 <<- as.data.frame(LR_RF[2])
    get_case1_plots(LR1,RF1,'nVar','TPR')
  })
  
  # CASE1 Plots
  output$case1_chart2 <- renderPlot({
    get_case1_plots(LR1,RF1,'nVar','Accuracy')
  })
  
  # CASE1 Plots
  output$case1_chart3 <- renderPlot({
    get_case1_plots(LR1,RF1,'nVar','F1')
  })
  
  # Case 1 Box plots
  output$case1_chart4 <- renderPlot({
    get_case1_boxplots()
  })
  
  ####################### Number of noise variables ************************************
  output$lr_title_num_nvar <- renderText({
    paste0( "Total Number of Explanatory Variables: ", input$n_sim)
  })
  
  output$rf_title_num_nvar <- renderText({
    paste0( "Total Number of Explanatory Variables: ", input$n_sim)
  })
  
  lr_rf_num_nvar <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_num_nvar(input$n_sim,
                                              input$split,
                                              input$cat,
                                              input$nrows,
                                              noise,
                                              input$ndist,
                                              input$nvar,input$ev,
                                              input$weights,
                                              input$yint,
                                              input$varselect, # Logistic Regression
                                              input$ntree # Random Forest
                 )
    )
  })
  
  # Plots
  output$casen_chart1 <- renderPlot({
    LR2 <<- as.data.frame(LR_RF_2[1])
    RF2 <<- as.data.frame(LR_RF_2[2])
    get_case1_plots(LR2,RF2,'Num.Noise','TPR')
  })
  
  # Plots
  output$casen_chart2 <- renderPlot({
    get_case1_plots(LR2,RF2,'Num.Noise','Accuracy')
  })
  
  # Plots
  output$casen_chart3 <- renderPlot({
    get_case1_plots(LR2,RF2,'Num.Noise','F1')
  })
  
  # Box Plots
  output$casen_chart4 <- renderPlot({
    get_casen_boxplots()
  })
  
  
  
  
  
  ############################# OTher ###################################
  
  
  # CASE 2 Variable SELECTION
  lr_rf_var <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- lr_rf_varselection(input$n_sim,
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
  
  # Print LR Matrix
  output$lr_sim_num_nvar <- renderTable({
    LR_RF_2 <<- lr_rf_num_nvar()
    LR_RF_2[1]
  })
  
  # Print RF Matrix
  output$rf_sim_num_nvar <- renderTable({
    LR_RF_2[2]
  })
  
  # Print output
  #output$rf_var <- renderPrint({print(lr_rf_var())})
}