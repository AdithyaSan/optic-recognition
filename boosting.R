#Author: AdithyaSan
#Created on Sun Mar 12 12:15:28 2017


#Loading the required libraries
library(dplyr)
library(adabag)

#Reading the files train and test data
digitsdata <- read.csv("B:/Spring 2017/ML/optdigits_raining.csv", header=FALSE) %>% as.data.frame()
testdata <- read.csv("B:/Spring 2017/ML/optdigits_test.csv", header=FALSE) %>% as.data.frame()

#View the data imported
View(digitsdata)
View(testdata)

attach(digitsdata)

digitsdata$V65 <- as.factor(digitsdata$V65)


#Boosting
#1st model
ada1 <- boosting(V65~., data = digitsdata, boos = TRUE, control = rpart.control(minsplit=0))
summary(ada1)
ada1$trees
ada1$importance
errorevol(ada1, digitsdata)
pred1 <- predict(ada1, testdata)  #Prediction on test data
summary(pred1)
pred1

#2nd model
ada2 <- boosting(V65~., data = digitsdata, boos = TRUE, mfinal = 25, control = rpart.control(minsplit=0))
summary(ada2)
ada2$trees
ada2$importance
errorevol(ada2, digitsdata)
pred2 <- predict(ada2, testdata)
summary(pred2)
pred2

#3rd model
ada3 <- boosting(V65~., data = digitsdata, boos = TRUE, mfinal = 300, control = rpart.control(minsplit=0))
summary(ada3)
ada3$trees
ada3$importance
errorevol(ada3, digitsdata)
pred3 <- predict(ada3, testdata)
summary(pred3)
pred3

#4th model
ada4 <- boosting(V65~., data = digitsdata, boos = TRUE, mfinal = 120, control = rpart.control(minsplit=0))
summary(ada4)
ada4$trees
ada4$importance
errorevol(ada4, digitsdata)
pred4 <- predict(ada4, testdata)
summary(pred4)
pred4

