getwd()
setwd("C:\\Users\\dharm\\Desktop\\Fall 20 SEM3\\IS 777\\Project")
logistic <- read.csv("b_depressed.csv",header=T)
head(logistic)
tail(logistic)


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

nrow(logistic) #No of Rows
ncol(logistic) #No of Columns
head(logistic$depressed)
str(logistic) # Variable Types of Col
colnames(logistic) # Column name in dataset



#dropping cols
logistic$Survey_id <- NULL #Survey id columns as it is not essential and do not contribute to the output
logistic$Ville_id <- NULL #Removing id columns as it is not essential and do not contribute to the output
head(logistic)

#checking for missing values and null values and na values
introduce(logistic)

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

# Checking if null values are removed and replaced
introduce(logistic)

logistic$no_lasting_investmen # checking if the NA values are replaced by column mean values

cor(logistic)

#descriptive analysis
summary(logistic)


# Checking correlation among data
plot_correlation(na.omit(logistic), maxcat = 5L)

#Histogram for Data
plot_histogram(logistic)



#Correlation of the variables
#Extract all the numeric columns from logistic
newdata <- logistic[, sapply(logistic, class) != "factor"]
head(newdata)

#Using correlation cor()
cor(newdata)
cor(non_fact_data)

#heat map
rel <- cor(newdata)
rel <- as.matrix(rel)
plot(rel)
pairs(rel)


str(logistic)
#converting qualitative variables into factor variables (correction for deliverables 2)
non_fact_data <- logistic
#converting the binary variables into factors
names(logistic)
logistic$sex <- as.factor(logistic$sex)
logistic$Married <- as.factor(logistic$Married)
logistic$incoming_salary <- as.factor(logistic$incoming_salary)
logistic$incoming_own_farm <- as.factor(logistic$incoming_own_farm)
logistic$incoming_business <- as.factor(logistic$incoming_business)
logistic$incoming_no_business <- as.factor(logistic$incoming_no_business)
logistic$labor_primary <- as.factor(logistic$labor_primary)

#converting the target variable into factor variable.
logistic$depressed <- as.factor(logistic$depressed)
str(logistic)


