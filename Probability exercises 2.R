library(tidyverse)
head(esoph)

all_cases <- esoph %>% filter(tobgp == "0-9g/day") %>% summarize(sum_cases=sum(ncases))
all_cases
all_controls <- esoph %>% filter(tobgp == "0-9g/day") %>% summarize(sum_controls=sum(ncontrols))
all_controls

cancer_high_alc <- esoph %>% filter(alcgp == "120+") %>% summarize(all_cases/(all_cases + all_controls)) 
cancer_high_alc

esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot)

esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot)

esoph  %>% summarize(sum_cases=all_cases, tot=sum(ncases), probability=(1-sum_cases/tot))

esoph  %>% summarize(sum_controls=all_controls, tot=sum(ncontrols), probability=(1-sum_controls/tot))

#################################################################################################################
all_cases <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum_cases=sum(ncases))
all_cases
all_controls <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum_controls=sum(ncontrols))
all_controls

esoph  %>% summarize(sum_cases=all_cases, tot=sum(ncases), probability=(sum_cases/tot))

esoph  %>% summarize(sum_controls=all_controls, tot=sum(ncontrols), probability=(sum_controls/tot))

#################################################################################################################


