avg <- 20.9
sd <- 5.7
ACT <- 1:36

set.seed(16)

act_scores <- rnorm(10000, avg, sd)

mean(act_scores)
sd(act_scores)

sum(act_scores>=36)

mean(act_scores > 30)

mean(act_scores <= 10)

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

z_scores <- rnorm(10000)

mean(z_scores>2)
2*sd(act_scores) + mean(act_scores)
qnorm(.95, avg, sd)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

