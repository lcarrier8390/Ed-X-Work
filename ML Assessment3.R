set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#1 is Disease, 0 is No Disease

mean(test == 1)
mean(test[disease==1])

mean(test == 0 & disease == 1)
mean(disease[test==1] == 1)/mean(disease == 1)
##############################################################################################################
library(dslabs)
data("heights")
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)
heights

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)