# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n

library(dslabs)
data(heights)

inches_to_ft <- function(x){
  n <- x
  n/12
}
inches_to_ft(144)

sum(ifelse(heights$height < 60, 1, 0))