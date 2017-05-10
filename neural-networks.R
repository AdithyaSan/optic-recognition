#Loading the required libraries
library(dplyr)
library(neuralnet)
library(nnet)

#Reading the files train and test data
digitsdata <- read.csv("Train data location", header=FALSE) %>% as.data.frame()
testdata <- read.csv("Test data location", header=FALSE) %>% as.data.frame()

#View the data imported
View(digitsdata)


#Vectorize the output label into 10 different label vectors
train <- cbind(digitsdata[, 1:64], class.ind(as.factor(digitsdata$V65)))
names(train) <- c(names(digitsdata)[1:64],"l0","l1","l2","l3","l4","l5","l6","l7","l8","l9")

test <- cbind(testdata[, 1:64], class.ind(as.factor(testdata$V65)))
names(test) <- c(names(testdata)[1:64],"l0","l1","l2","l3","l4","l5","l6","l7","l8","l9")

#View the processed train and test data
View(train)
View(test)

#Construct the formula for the nueralnet
n <- names(train)
formula <- as.formula(paste("l0+ l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 ~", 
                            paste(n[!n %in% c("l0","l1","l2","l3","l4","l5","l6","l7","l8","l9")], collapse = " + ")))
formula

#MODEL 1 ------------------------------------ 3 HIDDEN LAYERS --------------------------
#TRAIN MODEL 1
nn1 <- neuralnet(formula,
                 data = train,
                 hidden = c(32, 16, 8),
                 act.fct = "logistic",
                 linear.output = FALSE,
                 lifesign = "minimal")

plot(nn1)
predict1 <- compute(nn1, test[,1:64])

predicted1result <- predict1$net.result

#Accuracy of the model - Model 1
original_values <- max.col(test[, 65:74])
predicted1extracted <- max.col(predicted1result)
mean(predicted1extracted == original_values)


#MODEL 2 ------------------------------------ 2 HIDDEN LAYERS --------------------------
#TRAIN MODEL 2
nn2 <- neuralnet(formula,
                 data = train,
                 hidden = c(32, 16),
                 act.fct = "logistic",
                 linear.output = FALSE,
                 lifesign = "minimal")

predict2 <- compute(nn2, test[,1:64])

predicted2result <- predict2$net.result

original_values <- max.col(test[, 65:74])
predicted2extracted <- max.col(predicted2result)
mean(predicted2extracted == original_values)

#MODEL 3 ------------------------------------ 1 HIDDEN LAYER --------------------------
#TRAIN MODEL 3
nn3 <- neuralnet(formula,
                 data = train,
                 hidden = c(32),
                 act.fct = "logistic",
                 linear.output = FALSE,
                 lifesign = "minimal")

predict3 <- compute(nn3, test[,1:64])

predicted3result <- predict3$net.result

original_values <- max.col(test[, 65:74])
predicted3extracted <- max.col(predicted3result)
mean(predicted3extracted == original_values)


#VERIFICATION FOR MODEL 2

# Seed for reproducibility purposes
set.seed(500)
# t = 10 for 10 times validation
t <- 10
outs <- NULL
# Train-test split proportions
proportion <- 0.995 # Set to 0.995 for LOOCV

# Crossvalidate, go!
for(i in 1:t)
{
  index <- sample(1:nrow(train), round(proportion*nrow(train)))
  train_v <- train[index, ]
  test_v <- train[-index, ]
  nn_v <- neuralnet(formula,
                    data = train_v,
                    hidden = c(32, 16),
                    act.fct = "logistic",
                    linear.output = FALSE)
  
  # Compute predictions
  predictednn <- compute(nn_v, test_v[, 1:64])
  # Extracting results
  predictedResults <- predictednn$net.result
  # Accuracy
  original_values <- max.col(test_v[, 65:74])
  predictedResultExtracted <- max.col(predictedResults)
  outs[i] <- mean(predictedResultExtracted == original_values)
}

mean(outs)

