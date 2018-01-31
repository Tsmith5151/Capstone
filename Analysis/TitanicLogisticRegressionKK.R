# Set working directory
getwd()
setwd('/Users/kkirasich/Documents/GitHub/Capstone/Data/Titanic/')

# Load data
train <- read.csv('titanic_train.csv')

# View/ check data
head(train)
summary(train)


# Create training and validation data from given data
library(caTools)
set.seed(88)
split <- sample.split(train$Survived, SplitRatio = 0.75)

# Training and test data
subtrain <- subset(train, split == TRUE)
subtest <- subset(train, split == FALSE)
subtrain <- na.omit(subtrain)


# Logistic regression model
model <- glm (Survived~., data = subtrain, family = binomial)
summary(model)

# Predictions
predict <- predict(model, type = 'response')

# Confusion matrix
table(subtrain$Survived, predict > 0.5)

# ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, subtrain$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

# Plot glm
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

# # Try model with Pclass and Fare
# model2 = glm(Survived ~ . - Pclass - Fare, data = subtrain, family = binomial)
# summary(model2)
# 
# # Predict
# predict2 <- predict(model2, type = 'response')
# 
# # Confusion matrix
# table(subtrain$Survived, predict2 > 0.5)
# 
# #ROCR curve
# ROCRpred <- prediction(predict, subtrain$Survived)
# ROCRperf <- performance(ROCRpred, 'tpr','fpr')
# plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
