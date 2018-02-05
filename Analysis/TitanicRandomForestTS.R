library(randomForest)
library(dummies)


#################### Data Preprocessing ####################


prep_data <- function(df){
  
  # Impute Missing Values
  df$Age <- ifelse(is.na(df$Age), median(df$Age, na.rm=TRUE), df$Age)
  df$Fare <- ifelse(is.na(df$Fare), median(df$Fare, na.rm=TRUE), df$Age)
  
  # Convert to Dummy Variables
  df <- dummy.data.frame(df, names=c('Embarked','Sex','SibSp','Parch'), sep="_")
  
  # Exclude Columns
  df <- df[,!names(df) %in% c('Name','Ticket','Cabin','PassengerId')]
  
  # Return Training   
  return(df)
}

# Dataset
raw_train <- read.csv('Data/Titanic/titanic_train.csv')
test <- read.csv('Data/Titanic/titanic_test.csv')


#################### RANDOM FOREST MODEL ####################

# Hyperparameter Tuning Random Forest
rf_tune <- function(train){
  
  # Train Random Forest Model
  for(n_tree in seq(50,400,50)){
    
    fit <- randomForest(train[,2:24], as.factor(train$Survived), ntree=n_tree, importance=TRUE)
    plot(fit$err.rate[,1],type='l',xlab='No. Trees',ylab='Error Rate',main='RandomForest: Out of Bag Error Rate',col='blue',lwd=3)
    legend('topright',legend=paste0('No. Trees: ',n_tree),box.lty=0)
  }
  
}

rf_tune(train)


rf_main <-function(train,n_tree){
        
        
        fit <- randomForest(train[,2:24], as.factor(train$Survived), ntree=n_tree, importance=TRUE)
        print(fit)
        
        # Feature Importance --> Type:2 = decrease in node impurity
        print(importance(fit,type=2))
}

# Train Model w/ Optimal Number of Trees
rf_main(train,100)



