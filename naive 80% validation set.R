getwd()
setwd("C://Users//vaish//OneDrive//Desktop//Dharmil")

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
#hovalid set

naive.data <- naive.data[sample(nrow(naive.data)),]
select.datanaive <- sample (1:nrow(naive.data), 0.8*nrow(naive.data))
train.datanaive <- naive.data[select.datanaive,]
test.datanaive <- naive.data[-select.datanaive,]
test.naive <- test.datanaive
train.naive <- train.datanaive
length(train.naive$depressed)
dep <- train.naive$depressed
str(dep)
dep <- as.data.frame(dep)
nrow(dep)



head(train.naive)
nrow(train.naive)
nrow(test.naive)
str(naive.data)
nrow(naive.data)
nrow(train.naive$depressed)
head(train.naive$depressed)
#install.packages("e1071")
library(e1071)
library(caret)
library(dplyr)
acc.naive <- 0

for (i in 1:10) 
{ 
  print("variable update")
  print(i)
  train.naive$Age <- train.naive$Age*train.naive$Age
  train.naive$Number_children <-  train.naive$Number_children*train.naive$Number_children
  train.naive$education_level <- train.naive$education_level*train.naive$education_level
  train.naive$total_members <- train.naive$total_members*train.naive$total_members
  train.naive$gained_asset <- train.naive$gained_asset*train.naive$gained_asset
  train.naive$durable_asset <- train.naive$durable_asset*train.naive$durable_asset
  train.naive$save_asset <- train.naive$save_asset*train.naive$save_asset
  train.naive$living_expenses <- train.naive$living_expenses*train.naive$living_expenses
  train.naive$other_expenses <- train.naive$other_expenses*train.naive$other_expenses
  train.naive$incoming_own_farm <- train.naive$incoming_own_farm*train.naive$incoming_own_farm
  train.naive$incoming_agricultural <- train.naive$incoming_agricultural*train.naive$incoming_agricultural
  train.naive$farm_expenses <- train.naive$farm_expenses*train.naive$farm_expenses
  train.naive$lasting_investment <- train.naive$lasting_investment*train.naive$lasting_investment
  train.naive$no_lasting_investmen <- train.naive$no_lasting_investmen*train.naive$no_lasting_investmen
  train.naive$depressed <- train.naive$depressed
  print("variables updated")
  naive <- naiveBayes(depressed ~ Age+Number_children+
                        education_level+total_members+gained_asset+
                        durable_asset+save_asset+living_expenses+
                        other_expenses+incoming_salary+
                        incoming_agricultural+farm_expenses+
                        lasting_investment+no_lasting_investmen , 
                      data = train.naive,)
  print("naive built")
  print(i)
  print(naive)
}  

