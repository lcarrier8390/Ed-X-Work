library(gtools)
library(tidyverse)

set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)

#########################################################################################################

entre <- c("1", "2", "3", "4", "5", "6")
side1 <- c("1", "2", "3", "4", "5", "6")
side2 <- c("1", "2", "3", "4", "5", "6")
drink <- c("1", "2", "3")
combinations(6, 2) #15 different combos of sides

meal <- expand.grid(entre = entre, side1 = side1, side2 = side2, drink = drink)
meal
count(meal, side1 != side2)

combinations(6, 3)

######################################################

entre_choices <- function(x){
  x * nrow(combinations(6,2)) * 3
}

combos <- sapply(2:12, entre_choices)

data.frame(entres = 2:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entres)
#####################################################

# Write a function that takes a number of side choices and returns the number of
#  meal combinations possible given 6 entree choices, 3 drink choices, and a selection
#  of 2 sides from the specified number of side choices.
ff <- function(sides){
  3*6*nrow(combinations(sides,2))
}

# Use sapply to apply the function to side counts ranging from 2 to 12.
# What is the minimum number of side options required in order to generate
#  more than 365 combinations?
options <- 2:12
sapply(options, ff)
