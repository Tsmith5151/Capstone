library(simstudy)
library(ggplot2)
library(GGally)
library(randomForest)
library(ROCR)
library(caTools)
set.seed(1)


sim_data <- function(n_obs,n_noise,n_ev){
  
  data <- c()
  # Noise Variables
  for (i in 1:n_noise){
    data<- defData(data,varname=paste0('n',i), dist="normal", formula = "0", variance = .1, link = "identity")
  }
  
  # Explanatory Variables
  for (i in 1:n_ev){
    data<- defData(data,varname=paste0('EV',i), dist="normal", formula = "0", variance = .1, link = "identity")
  }

  # EV Names
  ev <- c()
  weights <- round(runif(n_noise+n_ev,0,1),1)
  for (i in (1:n_ev)){
          ev[[i]] <- (paste0(weights[[i]],'*EV',i,sample(c('-','+'),1)))
          }
 
  # Build Response Variable
  x <- (paste(unlist(ev), collapse=''))
  
  y_int = 0.10
  formula <- paste0(y_int,'+',x)
 
  data <- defDataAdd(data,varname="y", dist="binary", formula=substring(formula,1,nchar(formula)-1), link = "logit")
  
  # Build Simulated Data 
  data <- genData(n_obs,data)
  
  return(as.data.frame.matrix(data))
}


plot_data <- function(data){
  
  # Parameters:
  # ---------------
  # data: input simulated dataframe

  # Returns:
  # ---------------
  # Scatter Matrix of Simulated Dataset
  
  # Only plot continous variables
  data <-data[,2:(ncol(data)-1)]
  cols <- sapply(data,is.integer)
  data <- data[,!cols]
  
  p <- ggpairs(data,lower=list(continuous=wrap("smooth", colour="navy")),
               diag=list(continuous=wrap("barDiag", fill="darkcyan")))
  
  plot <- p + labs(title= paste("Simulated Dataset")) + theme(plot.title = element_text(size=14,face="bold")) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}


rf_model <- function(data,n_trees,spit=0.70){
  
  ## Test/Train Split
  split <- sample.split(data, SplitRatio =0.70)
  train <- subset(data, split == TRUE)
  test <- subset(data, split == FALSE)
  
  # Training
  x_train <- train[,2:(ncol(data)-1)]
  x_test <- test[,2:(ncol(data)-1)]

  # Labels
  y_train <- as.factor(train[,'y'])
  y_test <- as.factor(test[,'y'])
  
  # Fit Model
  fit <- randomForest(x_train, y_train, ntree=n_trees, importance=TRUE)
  plot(fit$err.rate[,1],type='l',xlab='No. Trees',ylab='Error Rate',main='RandomForest: Out of Bag Error Rate',col='blue',lwd=3)
  legend('topright',legend=paste0('No. Trees: ',n_trees),box.lty=0)
  
  # Y Predictions
  y_pred <- predict(fit, x_test)
  
  # Output Table
  conf_matrix <- table(observed = y_test, predicted = y_pred)
  acc <- (sum(diag(conf_matrix)) / length(y_test))
  return(acc)
}

#Simulate Data
data <- sim_data(100,2,4)

# Visualize Data
plot_data(data)

# RandomForest Model Simulation
results <- c()
for(i in 1:10){
  results <- rf_model(data,200)
}


