getwd()
setwd("C:\\Users\\dharm\\Desktop\\Fall 20 SEM3\\IS 777\\Project")

logistic <- read.csv(file = "b_depressed.csv")
head(logistic)

#install.packages('knitr')
library(knitr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages('ggplot2')
library(ggplot2)
#install.packages("mice")
#install.packages('mice')
library(mice)
#install.packages('lattice')
library(lattice)
#install.packages('reshape2')
library(reshape2)
#install.packages("DataExplorer") 
library(DataExplorer)

introduce(logistic)
#checking for missing values and null values and na values
colnames(logistic)[colSums(is.na(logistic)) > 0]
is.null(logistic)

#handling na with mean
#finding mean of no_lasting_investmen
logistic[is.na(logistic)] = 0
is.null(logistic$no_lasting_investmen)
colnames(logistic)[colSums(is.na(logistic)) > 0]
lapply(logistic,function(x) { length(which(is.na(x)))})

mean_nli <- mean(logistic$no_lasting_investmen)
mean_nli

#replacing na with mean
for (i in 1:nrow(logistic)) {
  if(logistic$no_lasting_investmen[i] == 0){
    logistic$no_lasting_investmen[i] <- mean_nli
  }
}


#Dropping ville id and survey id
logistic$Survey_id <- NULL
logistic$Ville_id <- NULL
head(logistic)
#create a list of random number ranging from 1 to number of rows from actual data 
#and 80% of the data into training data 
#logistic <- logistic[sample(nrow(logistic)),]
#select.data <- sample (1:nrow(logistic), 0.8*nrow(logistic))
#train <- logistic[select.data,]
#test <- logistic[-select.data,]


train <- logistic[1:1143,]
test <- logistic[1143:1430,]


head(train)
head(test)
head(test)
dim(train)
dim(test)

## fit a logistic regression model with the training dataset
log.model <- glm(depressed ~., data = train, family = binomial(link = "logit"))
summary(log.model)

## to predict using logistic regression model, probablilities obtained
log.predictions <- predict(log.model, test, type="response")

## Look at probability output
log.predictions

#Below we are going to assign our labels with decision rule that 
#if the prediction is greater than 0.5, assign it 1 else 0.
log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
log.prediction.rd

#confusion matrix
table(log.prediction.rd, test[,21])
#Accuracy
accuracy <- table(log.prediction.rd, test[,21])
sum(diag(accuracy))/sum(accuracy)

##FEATURE SELECTION----------------------------------------------
##0--------------------------------------------------------------
#plotting the correlation
plot_correlation(na.omit(logistic), maxcat = 5L)

f_logistic <- logistic
f_logistic$Survey_id <- NULL
f_logistic$Ville_id <- NULL
f_logistic$sex <- NULL
f_logistic$gained_asset <- NULL
f_logistic$durable_asset <- NULL
f_logistic$save_asset <- NULL
f_logistic$living_expenses <- NULL
f_logistic$other_expenses <- NULL
f_logistic$incoming_agricultural <- NULL
f_logistic$farm_expenses <- NULL
f_logistic$lasting_investment <- NULL
f_logistic$no_lasting_investmen <- NULL

#create a list of random number ranging from 1 to number of rows from actual data 
#and 80% of the data into training data 
#f_logistic <- f_logistic[sample(nrow(f_logistic)),]
#select.data <- sample (1:nrow(f_logistic), 0.8*nrow(f_logistic))
#train <- f_logistic[select.data,]
#test <- f_logistic[-select.data,]

#n <- floor(0.8*nrow(f_logistic))
#select.data <- f_logistic[1:n, ]
#train <- f_logistic[select.data,]
#test <- f_logistic[-select.data,]

train <- f_logistic[1:1143,]
test <- f_logistic[1143:1430,]

head(train)
head(test)
dim(train)
dim(test)

## fit a f_logistic regression model with the training dataset
log.model <- glm(depressed ~., data = train, family = binomial(link = "logit"))
summary(log.model)

## to predict using f_logistic regression model, probablilities obtained
log.predictions <- predict(log.model, test, type="response")

## Look at probability output
log.predictions

#Below we are going to assign our labels with decision rule that 
#if the prediction is greater than 0.5, assign it 1 else 0.
log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
log.prediction.rd

#confusion matrix
table(log.prediction.rd, test[,11])
#Accuracy
accuracy <- table(log.prediction.rd, test[,11])
sum(diag(accuracy))/sum(accuracy)


str(logistic)

logistic$depressed <- as.factor(logistic$depressed)


