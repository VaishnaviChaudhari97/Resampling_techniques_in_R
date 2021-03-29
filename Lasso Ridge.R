#install.packages('mlbench')
library(mlbench)
#install.packages('psych')
library(psych)
library(tidyverse)
library(caret)
#install.packages('leaps')
library(leaps)

library(MASS)
library(caret)
library(e1071)
library(glmnet)

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

str(logistic)

##### Backward
head(logistic)
## FitAll = glm(depressed ~ sex + Age + Married + Number_children + education_level + total_members + gained_asset + durable_asset + save_asset + living_expenses +  other_expenses + incoming_salary + incoming_own_farm + incoming_business + incoming_no_business + incoming_agricultural + farm_expenses + labor_primary + lasting_investment+ no_lasting_investmen, data = logistic)
FitAll <- glm(depressed ~ ., data = logistic)

summary(FitAll)

######3

FitAll <- glm(depressed ~ ., data = logistic, family = 'binomial')
formula(FitAll)

model.fits <- stepAIC(FitAll, direction = 'backward')

log.predictions <- predict(model.fits, logistic[,1:20], type="response")

log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)

head(logistic[,1:20])

accuracy <- mean(log.prediction.rd != logistic[,21])
print(paste('Accuracy',1 -accuracy ))



### Forward
## Only intercept no regression coefficient
Fitstart1 <- glm(depressed ~ 1, data = logistic, family='binomial')
head(Fitstart1)

summary(Fitstart1)
model.fits2 <- stepAIC(Fitstart1, direction = 'forward',scope=formula(FitAll))




log.predictions <- predict(model.fits2, logistic[,1:20], type="response")
log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
accuracy <- mean(log.prediction.rd != logistic[,21])
print(paste('Accuracy',1 -accuracy ))




coefficients(Fitstart1)
coefficients(model.fits2)

summary(model.fits2)



##### Lasso

x = model.matrix(depressed~.,logistic)[,-1]
head(x)
lasso.mod=glmnet(x, logistic[,21],alpha=1, family = 'binomial')


lasso.mod2=cv.glmnet(x, logistic[,21],alpha=1, family = 'binomial')
summary(lasso.mod2)
lambda = lasso.mod2$lambda.min
plot(lasso.mod)
lambda


lasso.mod=glmnet(x, logistic[,21],alpha=1, family = 'binomial', lambda = lambda)

coef(lasso.mod)

log.predictions <- predict(lasso.mod, newx = x)
log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
accuracy <- mean(log.prediction.rd != logistic[,21])
print(paste('Accuracy',1 -accuracy ))




###### Ridge
y = model.matrix(depressed~.,logistic)[,-1]
head(y)
ridge.mod=glmnet(x, logistic[,21],alpha=0, family = 'binomial')


ridge.mod2=cv.glmnet(x, logistic[,21],alpha=0, family = 'binomial')
summary(ridge.mod2)
lambda = ridge.mod2$lambda.min
plot(ridge.mod)
lambda


ridge.mod=glmnet(x, logistic[,21],alpha=0, family = 'binomial', lambda = lambda)

coef(ridge.mod)

log.predictions <- predict(ridge.mod, newx = y)
log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
accuracy <- mean(log.prediction.rd != logistic[,21])
print(paste('Accuracy',1 -accuracy ))



### Part B Non Linearity

#install.packages("MM4LMM")
#install.packages("nlme")
#install.packages("gam")
library(mgcv)
library(nlme)
library(gam)




logitgam1<-gam(I(depressed) ~ Married+s(Age,df=4)+s(Number_children,df=4)+s(education_level,df=4)+s(total_members,df=4) ,data=logistic,family=binomial)
summary(logitgam1)
plot(logitgam1,se=T)






