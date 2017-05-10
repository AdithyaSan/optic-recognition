#Author: AdithyaSan
#Created on Sun Mar 12 11:42:42 2017

#Loading the required libraries
library(dplyr)
library(class)

#Reading the files train and test data
digitsdata <- read.csv("Train data location", header=FALSE) %>% as.data.frame()
testdata <- read.csv("Test data location", header=FALSE) %>% as.data.frame()

#View the data imported
View(digitsdata)
View(testdata)

#trainlabels as a separate variable
trainlabels <- digitsdata$V65

#KNN MODEL 1 with 1 neighbor
knn1 <- knn(train = digitsdata, test=testdata, cl = trainlabels, k = 1)
summary(knn1)
table(knn1, testdata$V65)
mean(knn1 == testdata$V65) * 100 

#KNN Model 2 with 5 neighbors
knn2 <- knn(train = digitsdata, test=testdata, cl = trainlabels, k = 5)
summary(knn2)
table(knn2, testdata$V65)
mean(knn2 == testdata$V65) * 100 

#KNN model 3 with 7 neighbors 
knn3 <- knn(train = digitsdata, test=testdata, cl = trainlabels, k = 7)
summary(knn3)
table(knn3, testdata$V65)
mean(knn3 == testdata$V65) * 100 

#KNN model 4 with 10 neighbors considered
knn4 <- knn(train = digitsdata, test=testdata, cl = trainlabels, k = 10)
summary(knn4)
table(knn4, testdata$V65)
mean(knn4 == testdata$V65) * 100 


#Validation and plotting for a better accuracy
accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  prediction <- knn(train = digitsdata, test=testdata, cl = trainlabels, k = x)
  accuracy[x] <- mean(prediction == testdata$V65)
}

plot(k, accuracy, type = 'b')
