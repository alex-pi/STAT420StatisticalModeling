# Question 1

?pt
# p-values should consider both extremes, that is why 
# we multiple by 2
2 * pt(2.1, df = 5, lower.tail = FALSE)

# Question 2

cl <- 0.9
# We want the remaining 0.01 split evenly left and right.
# we can move the cl to 0.99 + 0.005 to get the
# positive value from the right
qt(0.95, df = 10 - 2)

# Or we can do it from the left 1 - 0.995 and make sure
# we change the resulting crit value to positive.
qt(0.05, df = 10 - 2, lower.tail = FALSE)
abs(qt(0.05, df = 10 - 2))

# Go back to the confidence level used
1 - (2 * pt(1.859548, df = 10 - 2, lower.tail = FALSE))


# Question 3
# based on the distribution followed by beta_1
Sxx <- 1.5
s_2 <- 4
beta_1_hat_sd <- sqrt(s_2 / Sxx)

1 - pnorm(4.2, mean = 4, sd = beta_1_hat_sd)


# Question 4

?faithful
ft <- faithful
head(ft)

ftm <- lm(ft$eruptions ~ ft$waiting)
ftm <- lm(eruptions ~ waiting, data = ft)
n <- nrow(ft)

Sxx <- sum((ft$waiting - mean(ft$waiting)) ^ 2)

# The SE[beta_1_hat] is s_e (estimated sigma, divided by Sxx)
SE_beta_1_hat <- summary(ftm)$sigma / sqrt(Sxx)

summary(ftm)$sigma

# s_e has the RSS or SSE in the numerator
sqrt(sum((ft$eruptions - ftm$fitted.values)^2) / (n - 2))

# Question 5

summary(ftm)$coef[1, "t value"]

# manually
SE_beta_0_hat <- summary(ftm)$sigma * sqrt((1/n) 
                + (mean(ft$waiting)^2 / Sxx))

t_val <- ftm$coef[1] / SE_beta_0_hat

# Question 6

summary(ftm)$coef[2, "t value"]
ftm$coefficients[2] / SE_beta_1_hat

# Question 7

p_val <- summary(ftm)$coef[2, 4]
p_val < 0.01

# get the p value from the t value
2 * pt(summary(ftm)$coef[2, "t value"],
       df = n - 2, lower.tail = FALSE)


# Question 8

confint(ftm, level = 0.9, parm = "(Intercept)")[1, 2]

# Question 9

cint <- confint(ftm, level = 0.95, parm = "ft$waiting")
cint[2] - ftm$coefficients[2]

# the hard way to answer:

crit <- abs(qt(0.025, df = n - 2))
crit * SE_beta_1_hat

# Question 10

predict(ftm, level = 0.9, newdata = data.frame(
  waiting = c(81)
), interval = "confidence")[1, "lwr"]

# Question 11

predict(ftm, level = 0.99, newdata = data.frame(
  waiting = c(72)
), interval = "prediction")[1, "upr"]











