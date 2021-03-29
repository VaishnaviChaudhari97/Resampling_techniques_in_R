getwd()
setwd("C:\\Users\\dharm\\Desktop\\Fall 20 SEM3\\IS 777\\Project")

mydata <- read.csv(file = "b_depressed.csv")
head(mydata)

#dropping cols
mydata$Survey_id <- NULL
mydata$Ville_id <- NULL
head(mydata)

#checking for missing values and null values and na values
colnames(mydata)[colSums(is.na(mydata)) > 0]
is.null(mydata)

#handling na with mean
#finding mean of no_lasting_investmen
mydata[is.na(mydata)] = 0
is.null(mydata$no_lasting_investmen)
colnames(mydata)[colSums(is.na(mydata)) > 0]
lapply(mydata,function(x) { length(which(is.na(x)))})

mean_nli <- mean(mydata$no_lasting_investmen)
mean_nli

#replacing na with mean
for (i in 1:nrow(mydata)) {
  if(mydata$no_lasting_investmen[i] == 0){
    mydata$no_lasting_investmen[i] <- mean_nli
  }
}

#converting the target variable into factor variable
mydata$depressed <- as.factor(mydata$depressed)

knn.data <- mydata
naive.data <- mydata

#-----------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------

#validation training set

names(naive.data)
naive.data$sex <- as.factor(naive.data$sex)
naive.data$Married <- as.factor(naive.data$Married)
naive.data$incoming_salary <- as.factor(naive.data$incoming_salary)
naive.data$incoming_own_farm <- as.factor(naive.data$incoming_own_farm)
naive.data$incoming_business <- as.factor(naive.data$incoming_business)
naive.data$incoming_no_business <- as.factor(naive.data$incoming_no_business)
naive.data$labor_primary <- as.factor(naive.data$labor_primary)
str(naive.data)

y = naive.data$depressed
x = naive.data[,c(1:10)]
select.datanaive <- sample (1:nrow(naive.data), 0.8*nrow(naive.data))
train.datanaive <- naive.data[select.datanaive,]
test.datanaive <- naive.data[-select.datanaive,]
test.naive <- test.datanaive
train.naive <- train.datanaive
length(train.naive$depressed)
dep <- train.naive$depressed
str(dep)
dep <- as.data.frame(dep)
#performing N-fold validation 
#install.packages('caret')
library(caret)
head(x)

model = train(x,y,"nb",trControl=trainControl(method="cv", number=10),na.action=na.pass)
print(model)
print("naive built")
print(i)
print(model)

#Predicting the output 
Predict <- predict(model,newdata = test.naive)
#Calculating the accuracy
confusionMatrix(model)
#plotting the importance of each variable
X <- varImp(model)
plot(X)