#Previously, we used logistic regression to predict sex based on height. 
#Now we are going to use knn to do the same. 
#Set the seed to 1, then use the caret package to partition the dslabs heights data into a training and test set of equal size. 
#Use the sapply() function to perform knn with k values of seq(1, 101, 3) and calculate F1 scores with the F_meas() function 
#using the default value of the relevant argument.
library(dslabs)
library(caret)
library(tidyverse)
data("heights")
set.seed(1)
x <- heights$height
y <- heights$sex
n <- seq(1, 101, 3)

test_index <- createDataPartition(y, times = 1, p = .5, list = FALSE)
test <- heights[test_index,]
train <- heights[-test_index,]

F_1 <- sapply(n, function(k){
  fit <- knn3(sex ~ height, data = train, k = k)
  y_hat <- predict(fit, test, type = "class") %>% factor(levels = levels(train$sex))
  F_meas(data = y_hat, reference = test$sex)
  })

plot(n, F_1)

max(F_1)

n[which.max(F_1)]

##########################################################################################

library(dslabs)
library(caret)
Library(tidyverse)
data("tissue_gene_expression")
set.seed(1)
n <- seq(1, 11, 2)

testindex_gene <- createDataPartition(tissue_gene_expression$y, times=1, p=0.5, list=FALSE)

train_set_x <- tissue_gene_expression$x[-testindex_gene,] #matrix
train_set_y <- tissue_gene_expression$y[-testindex_gene] #factor

test_set_x <- tissue_gene_expression$x[testindex_gene,] #matrix
test_set_y <- tissue_gene_expression$y[testindex_gene] #factor

trainset_gene <- list('x' = train_set_x, 'y' = train_set_y)
testset_gene <- list('x' = test_set_x, 'y' = test_set_y)

F_1 <- sapply(n, function(k){
  fit <- knn3(y ~ x, data=trainset_gene, k=4)
  y_hat <- predict(fit, testset_gene, type = "class") %>%
    factor(levels = levels(trainset_gene$y))
  F_meas(data=y_hat, reference=testset_gene$y)
})

plot(n, F_1)

F_1

n[which.max(F_1)]
