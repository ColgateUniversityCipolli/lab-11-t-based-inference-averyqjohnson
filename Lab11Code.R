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
