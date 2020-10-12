# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
head(brexit_polls)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

brexit_polls[1,]

se_hat <- sqrt(.52*(1-.52)/4772)
se_hat
ce <- qnorm(.975)
ce
ci <-  c(.52 - ce*se_hat, .52 + ce*se_hat)
ci