getwd()
setwd("C:\\Users\\dharm\\Desktop\\Fall 20 SEM3\\IS 777\\Project")

logistic <- read.csv(file = "b_depressed.csv")
head(logistic)
library(knitr)
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#install.packages("mice")
library(mice)
library(lattice)
library(reshape2)
#install.packages("DataExplorer") 
library(DataExplorer)
introduce(logistic)
#checking for missing values and null values and na values
colnames(logistic)[colSums(is.na(logistic)) > 0]
is.null(logistic)
#handling na with mean
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
#head(train)
#head(test)
#head(test)
#dim(train)
#dim(test)

## fit a logistic regression model with the training dataset
acc <- 0

loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

# A vector for collecting the errors.
cv.error=vector(mode="numeric",length=5)
# The polynomial degree
degree=1:10
# A fit for each degree
for(i in 1:10){
  glm.fit=glm(depressed ~ sex+poly(Age,i, raw=TRUE)+Married+poly(Number_children,i, raw=TRUE)+
                poly(education_level,i, raw=TRUE)+poly(total_members,i, raw=TRUE)+poly(gained_asset,i, raw=TRUE)+
                poly(durable_asset,i, raw=TRUE)+poly(save_asset,i, raw=TRUE)+poly(living_expenses,i, raw=TRUE)+
                poly(other_expenses,i, raw=TRUE)+incoming_salary+poly(incoming_own_farm,i, raw=TRUE)+
                incoming_business+incoming_no_business+
                poly(incoming_agricultural,i, raw=TRUE)+poly(farm_expenses,i, raw=TRUE)+labor_primary+
                poly(lasting_investment,i, raw=TRUE)+poly(no_lasting_investmen,i, raw=TRUE), data=train)
  cv.error[i]=loocv(glm.fit)
  log.predictions <- predict(glm.fit, test, type="response")
  log.predictions
  log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
  log.prediction.rd
  table(log.prediction.rd, test[,21])
  accuracy <- table(log.prediction.rd, test[,21])
  acc[i] <- sum(diag(accuracy))/sum(accuracy)
  #print(i, acc[i])
  
}
acc
plot(c(1:10),acc,type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "degree", ylab = "accuracy")
# The plot of the errors
plot(degree,cv.error,type="b")