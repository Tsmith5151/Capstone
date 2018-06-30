library(simstudy)
library(ggplot2)
library(GGally)
library(randomForest)
library(ROCR)
library(caTools)
library(corrplot)
library(gplots)
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
  print("In rf")
  # Fit Model
  model.train <- randomForest(as.factor(y)~.,train,ntree=n_tree, cutoff=c(0.5,1-0.5))
  
  # Prediction
  pred<-predict(model.train, test[, !names(test) %in% c("y")], type = 'response')
  #print(pred)
  pred <- prediction(as.numeric(pred), as.numeric(test$y))
  #print(test$y)
  return(pred)
}

# Logistic Regression
do_logisticregression <- function(train,test,varselect){
  
  # Fit Model
  model.train = step(lm(y ~., family = "binomial", data = train), direction = varselect, cutoff=0.5)
  
  # Prediction
  pred <- predict(model.train, test[, !names(test) %in% c("y")], type = 'response')
  pred <- prediction(pred, test$y)
  return(pred)
}

simulation_nvar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
  print("Running sim...")
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
      perf <- performance(lr_pred, "tpr", "fnr")
      cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                            tpr=perf@y.values[[1]])
      cutoffs <- subset(cutoffs[order(cutoffs$cut, decreasing=TRUE),], (cut < 0.52 & cut > 0.47))
      TP_LR <<- cutoffs

      
      perf <- performance(rf_pred, "tpr", "fnr")
      TP_RF <<- perf
      
      break
      
      # # FPR
      # fp <- performance(lr_pred, measure = "tnr")
      # #print("For variance = " + nvar + "; LR TNR: " + fp@y.values[[1]])
      # fp = fp@y.values[[1]][23]
      # lr_fpr <- c(lr_fpr, fp)
      # 
      # fp <- performance(rf_pred, measure = "tnr")
      # # print("For variance = " + nvar + "; RF TNR: " + fp@y.values[[1]])
      # fp = fp@y.values[[1]][2]
      # rf_fpr <- c(rf_fpr, fp)
      # 
      # # Precision/Recall
      # p <- performance(lr_pred,measure='prec')
      # p = p@y.values[[1]][23]
      # lr_precision <- c(lr_precision, p)
      # 
      # p <- performance(rf_pred,measure='prec')
      # p = p@y.values[[1]][2]
      # rf_precision <- c(rf_precision, p)
      # 
      # # Recall
      # rc <- performance(lr_pred, measure='rec')
      # rc = rc@y.values[[1]][23]
      # lr_recall <- c(lr_recall, rc)
      # 
      # rc <- performance(rf_pred, measure='rec')
      # rc = rc@y.values[[1]][2]
      # rf_recall <- c(rf_recall, rc)
      # 
      # # F1 Score
      # f <- performance(lr_pred,measure="f")
      # x = as.data.frame(f@y.values[[1]])
      # f <- max(tail(x,-1))
      # lr_f1 <- c(lr_f1,f)
      # 
      # f <- performance(rf_pred,measure="f")
      # x = as.data.frame(f@y.values[[1]])
      # f <- max(tail(x,-1))
      # rf_f1 <- c(rf_f1,f)
      # 
      # #Accuracy
      # a <- performance(lr_pred, measure = "acc")
      # a = a@y.values[[1]][23]
      # lr_acc <- c(lr_acc,a)
      # 
      # a <- performance(rf_pred, measure = "acc")
      # a = a@y.values[[1]][2]
      # rf_acc <- c(rf_acc,a)
      # 
      # # AUC
      # a <- performance(lr_pred,measure="auc")
      # a <- a@y.values[[1]]
      # lr_auc <- c(lr_auc, a)
      # 
      # a <- performance(rf_pred,measure="auc")
      # a <- a@y.values[[1]]
      # rf_auc <- c(rf_auc, a)

      
    }
    
    # Table
    #lr_matrix <- populateMatrix(lr_matrix, i, nvar, lr_tpr, lr_fpr, lr_precision, lr_recall, lr_f1, lr_acc, lr_auc, lr_cost) 
    #rf_matrix <- populateMatrix(rf_matrix, i, nvar, rf_tpr, rf_fpr, rf_precision, rf_recall, rf_f1, rf_acc, rf_auc, rf_cost)
    #saveAUCScores(lr_auc,rf_auc, nvar)
  }
  #result=list(lr_matrix, rf_matrix) 
  #return(result)
  return
}





#############################################################################################

simulation_nvar(10, 
                             0.7, 
                             0,
                             100,
                             5,
                             "normal",
                             1,10,
                             "1",
                             0.1,
                             "forward", # Logistic Regression
                             100 # Random Forest
)
