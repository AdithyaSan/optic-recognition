library(e1071)
library(dplyr)



digitsdata <- read.csv("B:/Spring 2017/ML/optdigits_raining.csv", header=FALSE) %>% as.data.frame()
testdata <- read.csv("B:/Spring 2017/ML/optdigits_test.csv", header=FALSE) %>% as.data.frame()

attach(digitsdata)

#Default Naive Bayes Model#
naive_model1 <- naiveBayes(as.factor(V65)~ ., data = digitsdata)
class(naive_model1)
summary(naive_model1)
print(naive_model1)
naive_model1

preds <- predict(naive_model1, testdata[,-1])
preds
conf_matrix <- table(pred = preds, true = testdata$V65)
conf_matrix

print(paste0('Accuracy: ', 100*sum( preds == testdata$V65 )/length(testdata$V65), '%'))


#####Tweaking Naive Bayes####
#laplace and threshold values#
naive_model2 <- naiveBayes(as.factor(V65)~ ., data = digitsdata, laplace = 3, threshold = 0.001, eps = 3)

preds2 <- predict(naive_model2, testdata[,-1])
preds2
conf_matrix2 <- table(pred = preds2, true = testdata$V65)
conf_matrix2

print(paste0('Accuracy: ', 100*sum( preds2 == testdata$V65 )/length(testdata$V65), '%'))

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
  naive <- naiveBayes(as.factor(train_v$V65) ~ ., data=train_v)
  
  #Compute predictions
  predicted <- predict(naive, test_v[, -1])
  
  #Accuracies
  outs[i] <- mean(predicted == test_v$V65)
}

#Cross validation accuracy
mean(outs)*100

