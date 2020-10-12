# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% 
  mutate(x_hat = (spread + 1)/2, se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_d = 2*sqrt(p*(1-p)/samplesize),
         lower = spread - qnorm(.975)*se_d, upper = spread + qnorm(.975)*se_d,
         hit = (lower <= -.038  & upper >= -.038))

june_polls %>% group_by(pollster) %>% summarize(proportion_hits = mean(hit), N = n(), t = poll_type, s = spread) 

boxplot(june_polls$poll_type, june_polls$spread)

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type %>% mutate(se_hat = 2*sqrt(p_hat*(1-p_hat)/N), lower = spread - qnorm(.975)*se_hat, upper = spread + qnorm(.975)*se_hat)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)


totals <- brexit_hit %>% group_by(poll_type, hit) %>% summarize(num = n()) %>% spread(poll_type, num)
totals
totals %>% select(-hit) %>% chisq.test()

odds_C <- totals[[2,2]]/sum(totals[[2]])/(totals[[1,2]] / sum(totals[[2]]))
odds_C

odds_A <- totals[[2,3]]/sum(totals[[3]])/(totals[[1,3]]/sum(totals[[3]]))
odds_A

odds_C/odds_A

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long
