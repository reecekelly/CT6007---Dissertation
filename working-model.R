normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

FScore <- function(TP, FP, FN) {
  TPS <- 2*TP
  return ((TPS) / ((TPS)+FP+FN))
}

Accuracy <- function(TP, FP, TN, FN) {
  return ((TP+TN)/(TP+TN+FP+FN))
}

ErrorRate <- function(TP, FP, TN, FN) {
  return (1-((TP+TN)/(TP+TN+FP+FN)))
}

Precision <- function(TP, FP) {
  return (TP/(TP+FP))
}

Recall <- function(TP, FN) {
  return (TP/(TP+FN))
}

FScore <- function(TP, FP, FN) {
  TPS <- 2*TP
  return ((TPS) / ((TPS)+FP+FN))
}

## Decision tree packages and libary
install.packages("C50") 
library(C50)

install.packages("rpart")
library(rpart)

## Installs the package for graphs etc
install.packages("gmodels")
library(gmodels)
library(class)

# Setup directory
setwd('D:/Documents/Github/CT6007---')
titanicdata <- read.csv("train.csv", na.strings=c("", "NA"), stringsAsFactors=FALSE)
#titanic_test <- read.csv("test.csv", stringsAsFactors=FALSE)

# changes sex column values to useable data entries
titanicdata$Sex <- sub("female", 1, titanicdata$Sex)
titanicdata$Sex <- sub("male", 0, titanicdata$Sex)

# Remove excess features
titanicdata = titanicdata[-c(1,4,7:9,11:12)]

## removes entries with n/a
titanicdata = na.omit(titanicdata)

titanicdata_mod <- titanicdata[,c(2:5,1)]

titanicdata_normal = as.data.frame(lapply(titanicdata[2,4:5], normalize))

# Changes a column into factor
titanicdata_mod$Survived <- as.factor(titanicdata_mod$Survived)

# Counting the size of the dataset
numberofrows <- nrow(titanicdata)
numberofcolumns <- ncol(titanicdata)

# create a random sample for training and test data
# use set.seed to use the same random number sequence
set.seed(12345)
titanic_rand <- titanicdata_mod[order(runif(numberofrows)), ]

# Splitting the data set 90% modeling - 10% testing
titanic_train <- titanic_rand[1:ceiling(numberofrows*0.9), ]
titanic_test <- titanic_rand[ceiling(numberofrows*0.9):numberofrows, ]

# Checking for equal split (around 30%)
prop.table(table(titanic_train$Survived))
prop.table(table(titanic_test$Survived))

# Decision Tree model creation
titanic_model <- C5.0(titanic_train[-5],titanic_train$Survived,trials = 17)

# Creating useable data for cross table
titanic_pred <- predict(titanic_model, titanic_test)

# Cross table to plot results from Decision Tree model
ctable <- CrossTable(titanic_test$Survived, titanic_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

print(fscore <- FScore(ctable$t[1,1],ctable$t[1,2],ctable$t[2,1]))
print(accuracy <- Accuracy(ctable$t[1,1],ctable$t[1,2],ctable$t[2,2],ctable$t[2,1]))