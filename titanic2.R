options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train  %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%   ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2)
  scale_y_continuous(trans = "log2")
  
  #plot 1 - survival filled by classx
  titanic %>%
    ggplot(aes(Survived, fill = Pclass)) +
    geom_bar()
  # plot 2 - survival filled by class with position_dodge
  titanic %>%
    ggplot(aes(Survived, fill = Pclass)) +
    geom_bar(position = position_dodge())
  #plot 3 - class filled by survival
  titanic %>%
    ggplot(aes(Pclass, fill = Survived), position = "fill") +
    geom_bar()
  
  titanic %>%
    ggplot(aes(Pclass, fill = Survived), position = "fill") +
     geom_bar() + 
     facet_wrap(~ Pclass) + 
     labs(y = "Portion Died/Lived", title = "Titanic Survival by Sex and Class")
  