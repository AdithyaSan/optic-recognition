library(kernlab)
library(dplyr)
library(ggplot2)
library(ROCR)


digitsdata <- read.csv("Train data location", header=FALSE) %>% as.data.frame()
testdata <- read.csv("Test data location", header=FALSE) %>% as.data.frame()

####SVM1 - Vanilladot - different C values ############

########c = 100 ###########
Vanillasvm1 <- ksvm(digitsdata$V65 ~ ., data=digitsdata, type='C-svc', kernel='vanilladot',C=100, scale=c() )
Vanillasvm1

attributes(Vanillasvm1)


vanilla1.prediction <- predict( Vanillasvm1, testdata[1:64] )
print(paste0('Accuracy: ', 100*sum( vanilla1.prediction == testdata$V65 )/length(testdata$V65), '%'))

print('Confusion Matrix: ');print(table( vanilla1.prediction, testdata$V65, dnn= c("prediction","reality") ))

########c = 300 ###########
Vanillasvm2 <- ksvm(digitsdata$V65 ~ ., data=digitsdata, type='C-svc', kernel='vanilladot',C= 300, scale=c() )
Vanillasvm2

attributes(Vanillasvm2)


vanilla2.prediction <- predict( Vanillasvm2, testdata[1:64] )
print(paste0('Accuracy: ', 100*sum( vanilla2.prediction == testdata$V65 )/length(testdata$V65), '%'))

print('Confusion Matrix: ');print(table( vanilla2.prediction, testdata$V65, dnn= c("prediction","reality") ))

########c = 25 ###########
Vanillasvm3 <- ksvm(digitsdata$V65 ~ ., data=digitsdata, type='C-svc', kernel='vanilladot',C= 25, scale=c() )
Vanillasvm3

attributes(Vanillasvm3)


vanilla3.prediction <- predict( Vanillasvm3, testdata[1:64] )
print(paste0('Accuracy: ', 100*sum( vanilla3.prediction == testdata$V65 )/length(testdata$V65), '%'))

print('Confusion Matrix: ');print(table( vanilla3.prediction, testdata$V65, dnn= c("prediction","reality") ))


######################################################
####SVM2 - rbfdot - different C values ############
######## c = 100 ############
rbfsvm1 <- ksvm(digitsdata$V65 ~ ., data=digitsdata, type='C-svc', kernel='rbfdot',C=100, scale=c() )
rbfsvm1

attributes(rbfsvm1)


rbf1.prediction <- predict( rbfsvm1, testdata[1:64] )
print(paste0('Accuracy: ', 100*sum( rbf1.prediction == testdata$V65 )/length(testdata$V65), '%'))

print('Confusion Matrix: ');print(table( rbf1.prediction, testdata$V65, dnn= c("prediction","reality") ))

######## c = 100 (nu-svc) ############
rbfsvm2 <- ksvm(digitsdata$V65 ~ ., data=digitsdata, type='nu-svc', kernel='rbfdot',C=100, scale=c() )
rbfsvm2

attributes(rbfsvm2)


rbf2.prediction <- predict( rbfsvm2, testdata[1:64] )
print(paste0('Accuracy: ', 100*sum( rbf2.prediction == testdata$V65 )/length(testdata$V65), '%'))

print('Confusion Matrix: ');print(table( rbf2.prediction, testdata$V65, dnn= c("prediction","reality") ))


################ CROSS VALIDATION ####################
# Seed for reproducibility purposes
set.seed(500)
# t = 5 for 5 times validation
t <- 5
outs <- NULL
# Train-test split proportions
proportion <- 0.95

# Crossvalidation!
for(i in 1:t)
{
  index <- sample(1:nrow(digitsdata), round(proportion*nrow(digitsdata)))
  train_v <- digitsdata[index, ]
  test_v <- digitsdata[-index, ]
  svm_v <- ksvm(train_v$V65 ~ ., data=train_v, type='C-svc', kernel='rbfdot',C=100, scale=c() )
  
  #Compute predictions
  predictedsvm <- predict(svm_v, test_v[, 1:64])
  
  #Accuracies
  outs[i] <- mean(predictedsvm == test_v$V65)
}

#Cross validation accuracy
mean(outs) * 100

