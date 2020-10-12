library(tidyverse)
library(dslabs)

data(death_prob)

p <- .015
n <- 1000
a <- 150000
l <- -150000
b <- 1150

#### Useful Formulas
# Expected values of a random variable
# ap + b(1- p)
# Expected value of the sum of n draws of a random variable
# n * (ap + b(1-p))
# Standard deviation of an urn with two values
# abs(b - a) * sqrt(p(1 - p))
# Standard error of the sum of n draws of a random variable
# sqrt(n) * abs(b - a) * sqrt(p(1 - p))

avg <- n * (-a*p + b*(1-p))
se <- sqrt(n) * abs(b - -a) * sqrt(p*(1 - p))
pnorm(0, avg, se)
pnorm(-1000000, avg, se)

p <- seq(0.01, 0.03, 0.001)
f <- function(p){
  mu <- 1000 * (-150000*p + 1150*(1-p))
  err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p*(1 - p))
  pnorm(-1000000, mu, err)
}
sapply(p, FUN=f)

#############################################################

set.seed(25)
n <- 1000
p_loss <- 0.015

X <- sample(c(0,1), n, replace=TRUE, prob=c((1-p_loss),p_loss))
loss <- -150000*sum(X==1)/10^6 # in millions
profit <- 1150*sum(X==0)/10^6
loss+profit

set.seed(27)
S <- replicate(10000, {
  X <- sample(c(0, 1), 1000, replace=TRUE, prob=c((1-0.015), 0.015))
  loss <- -150000*sum(X==1)/10^6 # in millions
  profit <- 1150*sum(X==0)/10^6
  loss+profit
})
sum(S<=-1)/10000
