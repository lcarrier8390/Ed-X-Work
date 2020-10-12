library(tidyverse)
library(caret)
set.seed(1)
options(digits = 3)

n <- c(100, 500, 1000, 5000, 10000)

wow <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
})
    c(avg = mean(rmse), sd = sd(rmse))
}

we <- sapply(n, wow)
we

##############################################################################
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)
##############################################################################
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
mean(rmse)
sd(rmse)

################################################################################
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_1, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)


#############################
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_2, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)

#############################
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_1 + x_2, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)

###################################################################################
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_1, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)

set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_2, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)

set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_1 + x_2, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)