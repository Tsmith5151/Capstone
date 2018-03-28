library(Hmisc)
library("caret")
library("rpart")
library("tree")
library("e1071")
library(ggplot2) # Data visualization
library(randomForest)
library(readr) # CSV file I/O, e.g. the read_csv function
library(ROCR)
library(AUC)
library(caTools)
library(compare)
library(tidyr)
library(dyplr)
set.seed(1)

simdata <- read.csv("SimData.csv")
modelfeatures <- read.csv("ModelFeatures.csv")

#head(simdata)
#head(modelfeatures)

##Output

#Misclassification Rate ("MCR")
RFtab <- table(simdata$RFC, simdata$Y)
LRtab <- table(simdata$LRC, simdata$Y)
#RFtab
#LRtab

RFResults <- confusionMatrix(RFtab)
LRResults <- confusionMatrix(LRtab)
#RFResults
#LRResults

as.table(RFResults)
#as.matrix(RFResults)
#as.matrix(RFResults, what = "overall")
#as.matrix(RFResults, what = "classes")
as.table(LRResults)
#as.matrix(LRResults)
#as.matrix(LRResults, what = "overall")
#as.matrix(LRResults, what = "classes")

RFMCR <- 1-sum(diag(RFtab))/sum(RFtab)
LRMCR <- 1-sum(diag(LRtab))/sum(LRtab)
#RFMCR
#LRMCR

#prSummary(simdata)
#summary(simdata)




#AUC ("AUC")
auc(sensitivity(simdata$RFC,simdata$Y))
auc(specificity(simdata$LRC,simdata$Y))
auc(accuracy(simdata$RFC,simdata$Y))
auc(accuracy(simdata$LRC,simdata$Y))
auc(roc(simdata$RFC,simdata$Y))
auc(roc(simdata$LRC,simdata$Y))
roc(simdata$RFC,simdata$Y)
roc(simdata$LRC,simdata$Y)
plot(sensitivity(simdata$RFC,simdata$Y))
plot(specificity(simdata$RFC,simdata$Y))
plot(accuracy(simdata$RFC,simdata$Y))
plot(roc(simdata$RFC,simdata$Y))



#Confusion Table - True Positives ("TP") True Negatives ("TN") False Positives ("FP") False Negatives ("FN")
confusionMatrix(simdata$RFC,simdata$Y)
confusionMatrix(simdata$LRC,simdata$Y)

#Sensitivity ("Sensitivity")
sensitivity(simdata$RFC,simdata$Y)
sensitivity(simdata$LRC,simdata$Y)

#Specificity ("Specificity")
specificity(simdata$RFC,simdata$Y)
specificity(simdata$LRC,simdata$Y)

#Number of Type 1 and Type 2 Errors ("Type1", "Type2")
confusionMatrix(simdata$RFC,simdata$Y)
confusionMatrix(simdata$LRC,simdata$Y)


##Final Table not created yet

