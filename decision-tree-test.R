## Decision tree packages and libary
install.packages("C50") 
library(C50)

install.packages("rpart")
library(rpart)

# Setup directory
setwd('D:/Documents/Github/CT6007---')
titanicdata <- read.csv("train.csv", na.strings=c("", "NA"), stringsAsFactors=FALSE)
titanic_test <- read.csv("test.csv", stringsAsFactors=FALSE)
str(titanicdata)

PredictColumn <- "Survived"

# Counting the size of the dataset
numberofrows <- nrow(titanicdata)
numberofcolumns <- ncol(titanicdata)

# Remove excess features
titanicdata = titanicdata[-c(1,4,7,8,9,11,12)]

## removes entries with n/a
titanicdata = na.omit(titanicdata)

missingvalues <- sum(is.na(titanicdata[c(6)]))

(missingvalues/numberofrows)*100


for(i in 1:numberofcolumns){
  sum(is.na(titanicdata[c(i)]))
  percentage <- (sum(is.na(titanicdata[c(6)]))/numberofrows)*100
  
}

titanicdata$PredictColumn <- as.factor(titanicdata_mod$Survived)

# apply the cost matrix to the tree
titanic_survived <- C5.0(titanicdata[-5], titanicdata$PredictColumn, trials=15)

titanic_cost_pred <- predict(titanic_survived, titanic_test)

FScore <- function(TP, FP, FN) {
  TPS <- 2*TP
  return ((TPS) / ((TPS)+FP+FN))
}

print(fscore <- FScore(ctable$t[1,1],ctable$t[1,2],ctable$t[2,1]))