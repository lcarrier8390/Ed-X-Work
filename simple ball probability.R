beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 99999999999999999999999999999999999999999999    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

set.seed(1986)

beads <- rep(c("cyan", "magenta", "yellow"), times = c(3,5, 7))
sample(beads, 2) 
mean(beads == "cyan")