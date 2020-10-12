avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

x <- 1:100
avg(x)
identical(mean(x), avg(x))

#my_function <- function(VARIABLE_NAME){
  #perform operations on VARIABLE_NAME and calculate VALUE
  #VALUE
#}

# functions can have multiple arguments as well as default values
#avg <- function(x, arithmetic = TRUE){
  #n <- length(x)
  #ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
#}

compute_s_n <- function(n){
    x <- 1:n
    sum(x)
}
n <- 25
compute_s_n(n)

# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
    for(n in 1:m){
      s_n[n] <- compute_s_n(n)
    }

n <- 1:m
compute_s_n(m)
plot(n, s_n)