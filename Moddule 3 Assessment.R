n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")


n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)
#### Useful Formulas
# Expected values of a random variable
# ap + b(1- p)
# Expected value of the sum of n draws of a random variable
# n * (ap + b(1-p))
# Standard deviation of an urn with two values
# abs(b - a) * sqrt(p(1 - p))
# Standard error of the sum of n draws of a random variable
# sqrt(n) * abs(b - a) * sqrt(p(1 - p))

p <- death_prob %>% filter(age==50, sex=="Female") %>% pull(prob)
-150000*p + 1150*(1-p)
abs(-150000-1150)*sqrt(p*(1-p))
avg <- (1000*(-150000*p + 1150*(1-p)))
se <- sqrt(1000) * abs(1150 - -150000) * sqrt(p*(1 - p))
pnorm(0, 667378, 269657)

p <- death_prob %>% filter(age==50, sex=="Male") %>% pull(prob)

x <- ((700000/1000) - -150000*p)/(1-p)
avg <- (1000*(-150000*p + 1459*(1-p)))
se <- sqrt(1000) * abs(1459 - -150000) * sqrt(p*(1 - p))
pnorm(0, avg, se)
