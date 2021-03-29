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


 

boot_reg <- function(data, idx){
  fit <- glm(depressed~.,data = logistic[idx,], family = "binomial")
  coef(fit)
}

b <- boot::boot(logistic, boot_reg, 1000)
b

boot::boot.ci(b,index=1, type = "perc")



