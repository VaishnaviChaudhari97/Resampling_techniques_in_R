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
#hold out evaluation

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

for (i in 1:4) 
{ 
  print("variable update")
  print(i)
  naive.data$Age <- naive.data$Age*naive.data$Age
  naive.data$Number_children <-  naive.data$Number_children*naive.data$Number_children
  naive.data$education_level <- naive.data$education_level*naive.data$education_level
  naive.data$total_members <- naive.data$total_members*naive.data$total_members
  naive.data$gained_asset <- naive.data$gained_asset*naive.data$gained_asset
  naive.data$durable_asset <- naive.data$durable_asset*naive.data$durable_asset
  naive.data$save_asset <- naive.data$save_asset*naive.data$save_asset
  naive.data$living_expenses <- naive.data$living_expenses*naive.data$living_expenses
  naive.data$other_expenses <- naive.data$other_expenses*naive.data$other_expenses
  naive.data$incoming_own_farm <- naive.data$incoming_own_farm*naive.data$incoming_own_farm
  naive.data$incoming_agricultural <- naive.data$incoming_agricultural*naive.data$incoming_agricultural
  naive.data$farm_expenses <- naive.data$farm_expenses*naive.data$farm_expenses
  naive.data$lasting_investment <- naive.data$lasting_investment*naive.data$lasting_investment
  naive.data$no_lasting_investmen <- naive.data$no_lasting_investmen*naive.data$no_lasting_investmen
  naive.data$depressed <- naive.data$depressed
  print("variables updated")
  naive <- naiveBayes(depressed ~ Age+Number_children+
                        education_level+total_members+gained_asset+
                        durable_asset+save_asset+living_expenses+
                        other_expenses+incoming_salary+
                        incoming_agricultural+farm_expenses+
                        lasting_investment+no_lasting_investmen , 
                      data = naive.data,)
  print("naive built")
  print(i)
}  
print(naive)

#-------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#100% training dataset