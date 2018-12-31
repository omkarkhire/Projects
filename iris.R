# Setting up working directory
setwd("D:/Study/ML/AV-Proj/IRIS")

# Loading data
iris <- read.csv("iris.csv")

# Splitting data
library(caTools)
split <- sample.split(iris$Iris.setosa, SplitRatio = 0.7)
train<-subset(iris, split==TRUE)
test<-subset(iris, split==FALSE)

# Building CART
library(rpart)
library(rpart.plot)
cart1 <- rpart(Iris.setosa~., data=train, method="class")
pred_train <- predict(cart1, newdata=train, type="class")
table(train$Iris.setosa,pred_train)
# Accuracy = (34+33+34)/nrow(train) = 0.97

# Predicting on test set
pred_test <- predict(cart1, newdata = test, type="class")
table(test$Iris.setosa, pred_test)
# Accuracy = (15+15+12)/nrow(test) = 0.93


# Random forest
library(randomForest)
random1 <- randomForest(Iris.setosa~., data=train)
pred_train1 <- predict(random1, data=train)
table(train$Iris.setosa, pred_train1)
# Accuracy = (34+32+32)/nrow(train) = 0.94

pred_test1<- predict(random1, newdata=test)
table(test$Iris.setosa, pred_test1)
# Accuracy = (15+15+11)/nrow(test) = 0.91


# Cross Validation
library(caret)
library(e1071)

# Define how many folds we want
numfold = trainControl(method="cv", number=10)
cpGrid=expand.grid(.cp=seq(0.002,0.1,0.002))

train(Iris.setosa ~ ., data=train, method="rpart", trControl=numfold, tuneGrid=cpGrid)

cart2<- rpart(Iris.setosa ~ ., data=train, method="class",cp=0.1)
pred_test2 <- predict(cart2, newdata=test, type = "class")
table(test$Iris.setosa, pred_test2)
# Acuracy = (15+15+12)/nrow(test) = 0.93
