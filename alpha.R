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

## Decision tree packages and libary
install.packages("C50") 
library(C50)

install.packages("rpart")
library(rpart)

library(gmodels)

# Setup directory
setwd('D:/Documents/Github/CT6007---')
titanicdata <- read.csv("train.csv", na.strings=c("", "NA"), stringsAsFactors=FALSE)
titanic_test <- read.csv("test.csv", stringsAsFactors=FALSE)

#Setting up the prediction column name
PredictColumn <- "Survived"

#Pass a string variable to the dataset
#print(titanicdata[,PredictColumn])

# Remove excess columns
titanicdata = titanicdata[-c(1,4)]

# Counting the size of the dataset
numberofrows <- nrow(titanicdata)
numberofcolumns <- ncol(titanicdata)

for(i in 1:numberofcolumns){
  print(is.numeric(titanicdata[c(i)]))
  
}

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

# changes sex column values to useable data entries
titanicdata$Sex <- sub("female", 1, titanicdata$Sex)
titanicdata$Sex <- sub("male", 0, titanicdata$Sex)