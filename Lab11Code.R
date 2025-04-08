################################################################################
# Lab 11 Code
# Avery Johnson
################################################################################

################################################################################
# Question 1: power
################################################################################

library(tidyverse)

# install.packages("pwr")
library(pwr)
# ?pwr

power.result <- power.t.test(delta=0.65, sig.level=0.05, power=0.80,
                           type="one.sample", alternative="two.sided")

# gives us n = 20.58044

################################################################################
# Question 2: clean data
################################################################################

dat <- read.csv("data.csv")

dat.clean <- dat |>
  mutate(difference = closer_vals - further_vals)
view(dat.clean)

################################################################################
# Question 3: summarize data
################################################################################
library(e1071)

# summarize the further data
summarize.further <- dat.clean |>
  summarize(
    mean = mean(further_vals, na.rm=T),
    sd = sd(further_vals, na.rm=T)
  )

# summarize the closer data
summarize.closer <- dat.clean |>
  summarize(
    mean = mean(closer_vals, na.rm=T),
    sd = sd(closer_vals, na.rm=T)
  )


# summarize the differences
summarize.further <- dat.clean |>
  summarize(
    mean = mean(difference, na.rm=T),
    sd = sd(difference, na.rm=T)
  )

dat.long <- dat.clean |>
  pivot_longer(cols = everything(), names_to = "condition", values_to = "fluorescence")

ggplot(data=dat.long, aes(x=condition, y=fluorescence)) +
  geom_boxplot()+
  theme_bw() +
  geom_hline(yintercept = 0) + 
  xlab("Condition") +
  ylab("% Change in Fluorescence")

# optional challenge: reproduce figure 2(g)

################################################################################
# Question 4: conduct inferences
################################################################################
library(effectsize)

# close
hedges_result_closer <- hedges_g(x = dat.clean$closer_vals, mu = 0, alternative = "greater")
interpret_hedges_g(1.61)

help("hedges_g")

t_test_result_closer <- t.test(x=dat.clean$closer_vals, mu = 0, alternative = "greater")
t_test_result_closer.ci <- t.test(x=dat.clean$closer_vals, mu = 0, alternative = "two.sided")

t_stat_closer <- t_test_result_closer$statistic
p_val_closer <- t_test_result_closer$p.value
hedges_g_closer <- hedges_result_closer$Hedges_g
ci_low_closer <- t_test_result_closer.ci$conf.int[1]
ci_high_closer <- t_test_result_closer.ci$conf.int[2]

results.closer <- data.frame(
  t = c(t_stat_closer),
  p_value = c(p_val_closer),
  g = c(hedges_g_closer),
  CI_Lower = c(ci_low_closer),
  CI_Upper = c(ci_high_closer)
)

view(results.closer)

library(xtable)
results.closer.xtable <- xtable(results.closer)

# far
hedges_result_further <- hedges_g(x = dat.clean$further_vals, mu = 0, alternative = "less")
interpret_hedges_g(-1.51)

t_test_result_further <- t.test(x=dat.clean$further_vals, mu = 0, alternative = "less")
t_test_result_further.ci <- t.test(x=dat.clean$further_vals, mu = 0, alternative = "two.sided")

t_stat_further <- t_test_result_further$statistic
p_val_further <- t_test_result_further$p.value
hedges_g_further <- hedges_result_further$Hedges_g
ci_low_further <- t_test_result_further.ci$conf.int[1]
ci_high_further <- t_test_result_further.ci$conf.int[2]

results.further <- data.frame(
  t = c(t_stat_further),
  p_value = c(p_val_further),
  g = c(hedges_g_further),
  CI_Lower = c(ci_low_further),
  CI_Upper = c(ci_high_further)
)

view(results.further)

library(xtable)
results.further.xtable <- xtable(results.further)

# differences
hedges_result_difference <- hedges_g(x = dat.clean$difference, mu = 0, alternative = "two.sided")
interpret_hedges_g(1.65)

t_test_result_difference <- t.test(x=dat.clean$difference, mu = 0, alternative = "two.sided")

t_stat_difference <- t_test_result_difference$statistic
p_val_difference <- t_test_result_difference$p.value
hedges_g_difference <- hedges_result_difference$Hedges_g
ci_low_difference <- t_test_result_difference$conf.int[1]
ci_high_difference <- t_test_result_difference$conf.int[2]

results.difference <- data.frame(
  t = c(t_stat_difference),
  p_value = c(p_val_difference),
  g = c(hedges_g_difference),
  CI_Lower = c(ci_low_difference),
  CI_Upper = c(ci_high_difference)
)

view(results.difference)

library(xtable)
results.difference.xtable <- xtable(results.difference)

################################################################################
# Question 5: reverse engineer hypothesis test plot from lecture 20
################################################################################

