library(tidyverse)
library(dslabs)
data(murders)

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total), size = 1.5) + 
  geom_text(aes(population/10^6, total, label = abb), nudge_x = .05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population in Millions (Log Scale)") +
  ylab("Total Number of Murders (Log Scale)") +
  ggtitle("US Gun Murders in US 2010")

# change the size of the points
#p + geom_point(aes(population/10^6, total), size = 3) +
#  geom_text(aes(population/10^6, total, label = abb))
#
# move text labels slightly to the right
#p + geom_point(aes(population/10^6, total), size = 3) +
#  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)
#
# simplify code by adding global aesthetic
#p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
#p + geom_point(size = 3) +
#  geom_text(nudge_x = 1.5)
#
# local aesthetics override global aesthetics
#p + geom_point(size = 3) +
#  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

#Final Plot
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
p + geom_point(aes(col = region), size = 1.5)

#Define Average Murder Rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# basic line with average murder rate for the country
p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))    # slope is default of 1

# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 1.5)

p <- p + scale_color_discrete(name = "Region")    # capitalize legend title
