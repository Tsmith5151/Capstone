# Set working directory
getwd()
setwd('/Users/kkirasich/Documents/GitHub/Capstone/Data/Titanic/')

# Load data
train <- read.csv('titanic_train.csv')

# View/ check data
head(train)
summary(train)

# Clean
train$Age[is.na(train$Age)] = mean(train$Age, na.rm = TRUE)

# Independent/dependent variables
nonvars = c("PassengerId","Name","Ticket","Embarked","Cabin")
train = train[,!(names(train) %in% nonvars)]
str(train)

# MultiCollinearity
train$Sex = as.numeric(train$Sex)
cor(train)

# Create training and validation data from given data
library(caTools)
set.seed(88)
split <- sample.split(train$Survived, SplitRatio = 0.75)

# Training and test data
subtrain <- subset(train, split == TRUE)
subtest <- subset(train, split == FALSE)

# NAs
sum(is.na(subtrain)) # 0  but still omit in case
subtrain <- na.omit(subtrain)


# Logistic regression model
model <- glm (Survived~., data = subtrain, family = binomial)
summary(model)

# Predictions
predict <- predict(model, type = 'response')

# Confusion matrix
table(subtrain$Survived, predict >= 0.5)
#   FALSE TRUE
# 0   311   48
# 1    67  161

# ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, subtrain$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

# Plots 
library(ggplot2)
# vs class
ggplot(subtrain, aes(x=Pclass, y=Survived)) + geom_point() +
  stat_smooth(method="glm", family="binomial", se=FALSE)
# Survival rate decreases as class decreases

# vs Age
ggplot(subtrain, aes(x=Age, y=Survived)) + geom_point() +
  stat_smooth(method="glm", se=FALSE)
# Survival rate decreases as age decreases

# vs Fare
ggplot(subtrain, aes(x=Fare, y=Survived)) + geom_point() +
  stat_smooth(method="glm", se=FALSE)
# Survival rate increases drastically as fare increases

# Try model with Pclass and Fare
model2 = glm(Survived ~ . - Pclass - Fare, data = subtrain, family = binomial)
summary(model2)

# Predict
predict2 <- predict(model2, type = 'response')

# Confusion matrix
table(subtrain$Survived, predict2 > 0.5)
#   FALSE TRUE
# 0   310   49
# 1    71  157

#ROCR curve
ROCRpred <- prediction(predict2, subtrain$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))



#######################################################################
# From https://www.kaggle.com/jeremyd/titanic-logistic-regression-in-r
#######################################################################

# Step 1: Load + Clean Data
Train <- train
Test <- read.csv('titanic_test.csv')

Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)

Test$Sex = as.numeric(Test$Sex)

# Step 4: Build a Logistic Regression Model
TitanicLog1 = glm(Survived~., data = Train, family = binomial)
summary(TitanicLog1)

# Step 5: Revise Model
TitanicLog2 = glm(Survived ~ . - Parch, data = Train, family = binomial)
summary(TitanicLog2)

TitanicLog3 = glm(Survived ~ . - Parch - Fare, data = Train, family = binomial)
summary(TitanicLog3)

# Step 6: Test Accuracy of Model on Training Data

# always predict 0 (didn't survive)
baseAcur = 549 / (549 + 342)

predictTrain = predict(TitanicLog1, type = "response")
table(Train$Survived, predictTrain >= 0.5)
#   FALSE TRUE
# 0   408   71
# 1    87  217

predictTrain = predict(TitanicLog2, type = "response")
table(Train$Survived, predictTrain >= 0.5)
#   FALSE TRUE
# 0   405   74
# 1    89  215

#ROCR curve
ROCRpred <- prediction(predictTrain, Train$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

predictTrain = predict(TitanicLog3, type = "response")
table(Train$Survived, predictTrain >= 0.5)
#   FALSE TRUE
# 0   400   79
# 1    91  213

#ROCR curve
ROCRpred <- prediction(predictTrain, Train$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


accuracy = (244 + 458) / nrow(Train)
sensitivity = 244 / (244 + 98)
specificity = 458 / (458 + 91)

cat("accuracy: ", accuracy, " > ", "baseline: ", baseAcur)
# accuracy:  0.8965517  >  baseline:  0.6161616


# Step 7: Use Model to predict survivability for Test Data
predictTest = predict(TitanicLog3, type = "response", newdata = Test)

# no preference over error t = 0.5
Test$Survived = as.numeric(predictTest >= 0.5)
table(Test$Survived)
#  0  1 
# 68 40 

Predictions = data.frame(Test[c("PassengerId","Survived")])
write.csv(file = "TitanicPred", x = Predictions)