library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

average <- mean(stars$magnitude)
SD <- sd(stars$magnitude)
c(average = average, SD = SD)

stars %>%
  ggplot(aes(stars$magnitude)) +
  geom_density()

stars %>%
  ggplot(aes(stars$temp)) +
  geom_density()

stars %>%
  ggplot(aes(log10(temp), magnitude)) +
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse()

stars %>%
  ggplot(aes(temp, magnitude), label = "star" ) +
  geom_point() 

z <- stars %>% filter(type == "G")
z
