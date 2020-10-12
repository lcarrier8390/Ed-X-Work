library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

##############################################################################################
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, foo)
dataa <- sapply(predictions,max)
dataa

########################### QUESTION 9 ###########################
# Perform a simple search to find the cutoff that produces the highest accuracy,
#  predicting virginica if greater than the cutoff and versicolor otherwise.
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

########################### QUESTION 10 ###########################
# Given that we know the test data, we can treat it like we did our training data
#  to see if the same feature with a different cutoff will optimize our predictions.
#  Which feature best optimizes our overall accuracy?
foofoo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5], 2, foofoo)
dataa <- sapply(predictions, max)
dataa

########################### QUESTION 10 ###########################
plot(iris,pch=21,bg=iris$Species)
# Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train
#  dataset by using the seq function with increments of 0.1. Then, report the overall
#  accuracy when applied to the test dataset by creating a rule that predicts virginica
#  if Petal.Length is greater than the length cutoff OR Petal.Width is greater than
#  the width cutoff, and versicolor otherwise.
foofoofoo <- function(x){
  y_hat <- ifelse(test$Petal.Length>4.7 | test$Sepal.Length>6.7,'virginica','versicolor')
  mean(y_hat==test$Species)
}

# What is the overall accuracy for the test data now?
predictions <- apply(test[,-5], 2, foofoofoo)
dataa <- sapply(predictions, max)
dataa