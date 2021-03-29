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

#----------------------------------------------------------
#data preprocessing
#For  

#Normalization
#extract numerical variables as we have categorical columns
num.vars<-sapply(knn.data, is.numeric)
#normalize selected data using function scale
knn.data[num.vars] <-lapply(knn.data[num.vars], scale)
#min-max normalization to scale [0, 1]
knn.data[num.vars] <-apply(knn.data[num.vars], 2, FUN = function(x) (x - min(x))/(max(x)-min(x)))
head(knn.data)  

naive.data <- knn.data

# evaluation
knn.data <- knn.data[sample(nrow(knn.data)),]
select.dataknn <- sample (1:nrow(knn.data), 0.8*nrow(knn.data))
train.dataknn <- knn.data[select.dataknn,]
test.dataknn <- knn.data[-select.dataknn,]
test.knn <- test.dataknn
train.knn <- train.dataknn
head(train.knn)

train.knn$depressed <-NULL
test.knn$depressed <-NULL
train.def <- train.dataknn$depressed
test.def <- test.dataknn$depressed

install.packages('class')
library(class)
install.packages('caret')
library(caret)
pr <- knn(train.knn,test.knn,cl=train.def,k=13)
tab <- table(pr,test.def)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

##---------naive Bayes:-----------------------------------

#evaluation
naive.data <- naive.data[sample(nrow(naive.data)),]
select.datanaive <- sample (1:nrow(naive.data), 0.8*nrow(naive.data))
train.datanaive <- naive.data[select.datanaive,]
test.datanaive <- naive.data[-select.datanaive,]
test.naive <- test.datanaive
train.naive <- train.datanaive
head(train.naive)

install.packages("e1071")
library(e1071)

naive <- naiveBayes(train.naive$depressed~. , data = train.naive)
naive

#predict
#install.packages("caret")
library(caret)
pre_naive = predict(naive, test.naive)
head(pre_naive)

confusionMatrix(table(pre_naive,test.naive$depressed))

#--------------------------------------------------
#---------------------------------------------------
#FEATURE SELECTION
#
mydata$Survey_id <- NULL
mydata$Ville_id <- NULL
mydata$sex <- NULL
mydata$gained_asset <- NULL
mydata$durable_asset <- NULL
mydata$save_asset <- NULL
mydata$living_expenses <- NULL
mydata$other_expenses <- NULL
mydata$incoming_agricultural <- NULL
mydata$farm_expenses <- NULL
mydata$lasting_investment <- NULL
mydata$no_lasting_investmen <- NULL

fknn <- mydata
fnaive <- mydata


## NAIVE BAYES---------------------------------------------

# evaluation after removing low correlated
fnaive <- fnaive[sample(nrow(fnaive)),]
select.datanaive <- sample (1:nrow(fnaive), 0.8*nrow(fnaive))
train.datanaive <- fnaive[select.datanaive,]
test.datanaive <- fnaive[-select.datanaive,]
test.naive <- test.datanaive
train.naive <- train.datanaive
head(train.naive)

#install.packages("e1071")
library(e1071)

naive <- naiveBayes(train.naive$depressed~. , data = train.naive)
naive

#predict
#install.packages("caret")
library(caret)
pre_naive = predict(naive, test.naive)
head(pre_naive)
dim(pre_naive)

dim(test.naive$depressed)
confusionMatrix(table(pre_naive,test.naive$depressed))


str(mydata)

