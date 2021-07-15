# simulated data

sim_1 <- function(sample_size = 500) {
  x <- runif(n = sample_size) * 5
  y <- 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x, y)
}

sim_2 <- function(sample_size = 500) {
  x <- runif(n = sample_size) * 5
  y <- 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x, y)
}

sim_3 <- function(sample_size = 500) {
  x <- runif(n = sample_size) * 5
  y <- 3 + 5 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}

# generate good data
set.seed(42)
sim_data_1 <- sim_1()

#scatter plot with fitted slr
plot(y ~ x, data = sim_data_1, col = "gray", pch = 20,
     main = "Data from Model 1")
fit_1 <- lm(y ~ x, data = sim_data_1)
abline(fit_1, col = "darkorange", lwd = 3)

#fitted vs residuals
plot(fit_1$fitted.values, fit_1$residuals,  
     col = "gray", pch = 20,
     xlab = "Fitted", ylab = "Residuals",
     main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

# generate data with increasing variance
set.seed(42)
sim_data_2 <- sim_2()

#scatter plot with fitted slr
plot(y ~ x, data = sim_data_2, col = "gray", pch = 20,
     main = "Data from Model 2")
fit_2 <- lm(y ~ x, data = sim_data_2)
abline(fit_2, col = "darkorange", lwd = 3)

#fitted vs residuals
# This one has a very obvious non-constant variance
plot(fit_2$fitted.values, fit_2$residuals,  
     col = "gray", pch = 20,
     xlab = "Fitted", ylab = "Residuals",
     main = "Data from Model 2")
abline(h = 0, col = "darkorange", lwd = 2)


# generate data with "incorrect form"
set.seed(42)
sim_data_3 <- sim_3()

#scatter plot with fitted slr
plot(y ~ x, data = sim_data_3, col = "gray", pch = 20,
     main = "Data from Model 3")
fit_3<- lm(y ~ x, data = sim_data_3)
abline(fit_3, col = "darkorange", lwd = 3)

#fitted vs residuals
# the mean of Y is not a linear function of the predictor
plot(fit_3$fitted.values, fit_3$residuals,  
     col = "gray", pch = 20,
     xlab = "Fitted", ylab = "Residuals",
     main = "Data from Model 3")
abline(h = 0, col = "darkorange", lwd = 2)

# BP test

library(lmtest)
# high p-value means that we can't reject the H_0
# which was to assume constant variance.
bptest(fit_1)
# low p-value means we Reject the Null Hyp.
# which means we have reason to doubt constant variance
bptest(fit_2)
# high p-value again so variance seems constant
# the problem with this model is that violates the
# assumption of linear relation, so the residual
# are not centered at the mean.
bptest(fit_3)

# To check the normality assumption, histograms can help
# but it is not clear enough.

par(mfrow = c(1, 3))
hist(resid(fit_1),
     xlab = "Residuals",
     main = "Histogram of residuals, fit_1",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_2),
     xlab = "Residuals",
     main = "Histogram of residuals, fit_1",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_3),
     xlab = "Residuals",
     main = "Histogram of residuals, fit_1",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)

# A better way to check normality are
# QQ plots

qq_plot = function(e) {
  
  n = length(e)
  normal_quantiles = qnorm(((1:n - 0.5) / n))
  # normal_quantiles = qnorm(((1:n) / (n + 1)))
  
  # plot theoretical verus observed quantiles
  plot(normal_quantiles, sort(e),
       xlab = c("Theoretical Quantiles"),
       ylab = c("Sample Quantiles"),
       col = "darkgrey")
  title("Normal Q-Q Plot")
  
  # calculate line through the first and third quartiles
  slope     = (quantile(e, 0.75) - quantile(e, 0.25)) / (qnorm(0.75) - qnorm(0.25))
  intercept = quantile(e, 0.25) - slope * qnorm(0.25)
  
  # add to existing plot
  abline(intercept, slope, lty = 2, lwd = 2, col = "dodgerblue")
}

set.seed(42)
x <- rnorm(100, mean = 0, sd = 1)
par(mfrow = c(1,2))
qqnorm(x, col = "darkgrey")
qqline(x, lty = 2, lwd = 2, col = "dodgerblue")
qq_plot(x)

# How does a qq plot look for data with normal distribution?
set.seed(42)
par(mfrow = c(1,3))
qq_plot(rnorm(10))
qq_plot(rnorm(25))
qq_plot(rnorm(100))

# from a t dist
# the greater the sample size the easiest
# is to spot when data is not normally distributed
set.seed(42)
par(mfrow = c(1,3))
qq_plot(rt(10, df = 4))
qq_plot(rt(25, df = 4))
qq_plot(rt(100, df = 4))

set.seed(42)
par(mfrow = c(1,3))
qq_plot(rexp(10))
qq_plot(rexp(25))
qq_plot(rexp(100))

# back to the three models from above

par(mfrow = c(1,1))
qqnorm(resid(fit_1), main = "Normal QQ-Plot, fit 1", col = "darkgrey")
qqline(resid(fit_1), lty = 2, lwd = 2, col = "dodgerblue")

# Not constant variance. Technically data here follows a Normal
# distribution, but since the variance is not constant we see
# "fat tails" in the q-q plot
qqnorm(resid(fit_2), main = "Normal QQ-Plot but Variance not Constant, fit 2", col = "darkgrey")
qqline(resid(fit_2), lty = 2, lwd = 2, col = "dodgerblue")


# With the quadratic mean data it also appears that Normality
# is violated but again that is not the case.
qqnorm(resid(fit_3), main = "Not Normal QQ-Plot, fit 2", col = "darkgrey")
qqline(resid(fit_3), lty = 2, lwd = 2, col = "dodgerblue")

# Shapiro-wilk test
set.seed(42)
shapiro.test(rnorm(25))
shapiro.test(rexp(25))

# low P value. 
# So we reject the assumption that the data 
# could have been sampled from a normal distribution.
# In other words, the hyp here is to assume that
# the data has been sampled from a normal distribution.
shapiro.test(fit_2$residuals)
shapiro.test(fit_3$residuals)

mpg_hp_add <- lm(mpg ~ hp + am, data = mtcars)

# With little amount of data is hard to decide if 
# constant variance is violated...
plot(mpg_hp_add$fitted.values, mpg_hp_add$residuals,  
     col = "gray", pch = 20,
     xlab = "Fitted", ylab = "Residuals",
     main = "mpg_hp_add")
abline(h = 0, col = "darkorange", lwd = 2)

# relatively high p-value, for instance with alpha=0.05
# p < alpha so we Reject the Null Hyp, which means there is
# a violation of the constant variance assumption.
bptest(mpg_hp_add)

qqnorm(resid(mpg_hp_add), main = "QQ-Plot, mpg_hp_add", col = "darkgrey")
qqline(resid(mpg_hp_add), lty = 2, lwd = 2, col = "dodgerblue")

# High p-value, so we are not concerned about normality
# violations.
shapiro.test(mpg_hp_add$residuals)



























