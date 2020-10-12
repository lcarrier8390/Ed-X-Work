options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train  %>% filter(Sex == "male") %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

#plot 1 - survival filled by sex
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar()

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = .2, bw = .5)