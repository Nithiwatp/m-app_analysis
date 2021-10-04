#setting up your working directory
setwd("C:/Users/Tong/Desktop/Intro to R/m-app data analysis_github")

itu <- read.csv(file.choose(), header=T)
head(itu)
mean(complete.cases(itu)) # No missing value, it is equals to 1.

#see overview
summary(itu)
hist(itu$Age, col = "green")
hist(itu$IntExp, col = "yellow")
hist(itu$IntFreq, col = "red")

#Tranform data
#check class of DV
class(itu$ITU)
#change class from integer to factor
itu$ITU <- as.factor(itu$ITU)
itu$Gender <- as.factor(itu$Gender)
itu$Education <- as.factor(itu$Education)
itu$Income <- as.factor(itu$Income)
#check class again
str(itu)
# remove ID column
itu$ID <- NULL

#generate random number (80% of 358 =280)
#and create training(80%) and testing(20%) datasets
set.seed(19)
s <- sample(358,280)
s
itu_train <- itu[s,]
itu_train
itu_test <- itu[-s,]


#create an decision tree model using training dataset
install.packages("rpart") #Run only one time
library(rpart)
dtm <- rpart(ITU~., itu_train, method="class")
dtm

#export training dataset
df1 <- data.frame(itu_train)
df1
write.csv(df1, "training_dataset.csv")

#export testing dataset
df2 <- data.frame(itu_test)
df2
write.csv(df2, "testing_dataset.csv")

#see overview of decision tree
install.packages("rpart.plot") #Run only one time
library(rpart.plot)
rpart.plot(dtm, type =5, extra =8)


#Misclassification Table (MT) using the model with testing dataset
p1 <- predict(dtm, itu_test, type="class")
p1
head(p1)
head(itu)
MT <- table( Actualvalue = itu_test$ITU, p1)
MT
Misclassification <- 1-sum(diag(MT))/sum(MT)
Misclassification

#export p1 and MT 
dfp1 <- data.frame(p1, itu_test$ITU, itu_test$PEOU, itu_test$PU)
dfp1
write.csv(dfp1, "prediction.csv")

dfmt <- data.frame(MT)
dfmt
write.csv(dfmt, "misclassification_table.csv")
