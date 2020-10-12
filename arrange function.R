library(tidyverse)
library(dslabs)
data(murders)

murders %>% arrange(population) %>% head()

murders %>% arrange(desc(murder_rate)) %>% head()

murders %>% arrange(region, murder_rate) %>% head()

murders %>% arrange(desc(murder_rate)) %>% top_n(10)
