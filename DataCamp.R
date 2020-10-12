beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)
########################################################

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, {
  simulated_games <- sample(c(0, 1), 6, replace = TRUE)
  sum(simulated_games)
})

# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.
mean(results>=4)

##########################################################################

# Assign a variable 'n' as the number of remaining games.
n <-  c("2", "3", "4", "5", "6", "7")

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c("0","1")

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l <- replicate(6, {simulated_games <- sample(outcomes, 6, replace = TRUE, prob = c(0.5, 0.5))})

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- data.frame(l)
possibilities
# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- colSums(possibilities == 1)
results
# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results>=4)

####################################################
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 6, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
prob_win(.5)