#Importing dplyr library
library(dplyr)

#Importing train and test data files from local storage
traindata <- read.csv("Training data location", header=FALSE) %>% as.data.frame()
testdata <- read.csv("Test data location", header=FALSE) %>% as.data.frame()

#Viewing train and test data that was imported
View(traindata)
View(testdata)
attach(traindata)

#Importing rpart library for recursive partitioning tree model
library(rpart)


mean(traindata$V2)

tmodel1 <- rpart(V65~., traindata, method="class")
plot(tmodel1)
text(tmodel1)
printcp(tmodel1)

tmodel2 <- rpart(V65~., traindata, method="class", control = rpart.control(cp=0.001))
plot(tmodel2)
text(tmodel2)
printcp(tmodel2)

tmodel3 <- rpart(V65~., traindata, method="class", control = rpart.control(cp=0.00001))
plot(tmodel3)
text(tmodel3)
printcp(tmodel3)

#Pruning model 1
prune1<- prune(tmodel1, cp=   tmodel1$cptable[which.min(tmodel1$cptable[,"xerror"]),"CP"])
plot(prune1)
text(prune1)
printcp(prune1)

#Pruning model 2
prune2<- prune(tmodel2, cp=   tmodel2$cptable[which.min(tmodel2$cptable[,"xerror"]),"CP"])
plot(prune2)
text(prune2)
printcp(prune2)

#Pruning model 3
prune3<- prune(tmodel3, cp=   tmodel3$cptable[which.min(tmodel3$cptable[,"xerror"]),"CP"])
plot(prune3)
text(prune3)
printcp(prune3)



#Prediction on testdata set
pred <- predict(prune1, testdata, type="class")
tab1 <- table(pred, testdata$V65)
sum_diag <- diag(tab1)
n <- sum(tab1)
n
accuracy1 <- sum(sum_diag)/ n
accuracy1
summary(pred)

#Second prediction on testdata using model 2
pred2 <- predict(prune2, testdata, type="class")
tab2 <- table(pred2, testdata$V65)
n2 <- sum(tab2)
accuracy2 <- sum(diag(tab2)) / n2
accuracy2

summary(pred2)

#Third prediction on testdata using model 3
pred3 <- predict(prune3, testdata, type="class")
tab3 <- table(pred3, testdata$V65)
n3 <- sum(tab3)
accuracy3 <- sum(diag(tab3)) / n3
accuracy3

summary(pred3)


