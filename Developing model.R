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

#Setting up the prediction column name
PredictColumn <- "Survived"

# Remove excess columns
titanicdata = titanicdata[-c(1,4)]

# Counting the size of the dataset
numberofrows <- nrow(titanicdata)
numberofcolumns <- ncol(titanicdata)

##Runs through data set and deletes columns with 10% or more n/a values
delete <- c()

for(i in 1:numberofcolumns) {
  print(percentage <- (sum(is.na(titanicdata[c(i)]))/numberofrows)*100)
  
  if(percentage > 10) {
    #print("to much")
    
    #titanicdata = titanicdata[-c(i)]
    delete <- union(delete, c(i))
  }
}

titanicdata = titanicdata[-delete]

## removes rows with n/a value
titanicdata = na.omit(titanicdata)
numberofrows <- nrow(titanicdata)

##Plots choosen column index value
chosencolumn <- match(PredictColumn,names(titanicdata))

#titanicdata <- titanicdata[,c(2:8,1)]

if(chosencolumn == 1) {
  print("yes")
  titanicdata <- titanicdata[,c(2:as.integer(numberofcolumns),1)]
} else {
  print("nah")
  titanicdata <- titanicdata[,c(1:chosencolumn-1,chosencolumn+1:numberofcolumns-1,chosencolumn)]
  
}

# Changes a column into factor
titanicdata[,PredictColumn] <- as.factor(titanicdata[,PredictColumn])

# create a random sample for training and test data
# use set.seed to use the same random number sequence
set.seed(12345)
titanicdata <- titanicdata[order(runif(numberofrows)), ]

# Splitting the data set 90% modeling - 10% testing
titanic_train <- titanicdata[1:ceiling(numberofrows*0.9), ]
titanic_test <- titanicdata[ceiling(numberofrows*0.9):numberofrows, ]

# Decision Tree model creation
titanic_model <- C5.0(titanic_train[-chosencolumn], titanic_train$Survived, trials = 17)

# Creating useable data for cross table
titanic_pred <- predict(titanic_model, titanic_test)

# Cross table to plot results from Decision Tree model
ctable <- CrossTable(titanic_test$Survived, titanic_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

print(fscore <- FScore(ctable$t[1,1],ctable$t[1,2],ctable$t[2,1]))
