
stop_dist_model <- lm(dist ~ speed, data = cars)
summary(stop_dist_model)

# extract summary information
names(summary(stop_dist_model))
summary(stop_dist_model)$coefficients[2, 4]

# basically is this p value is smaller than the alpha we
# specify (where?) it means we can reject the Null Hyp H_0
summary(stop_dist_model)$coefficients["speed", "Pr(>|t|)"]

# store test statistics
(beta_0_hat_t <- summary(stop_dist_model)$coefficients["(Intercept)", "t value"])

# This t value is for test statistic to try to reject the Null Hyp (H_0)
(beta_1_hat_t <- summary(stop_dist_model)$coefficients["speed", "t value"])

# Verify t value for speed 
(beta_1_hat <- stop_dist_model$coefficients[2])
Sxx <- sum((cars$speed - mean(cars$speed)) ^ 2)
(s_e <- summary(stop_dist_model)$sigma)
manual_beta_1_hat_t <- (beta_1_hat - 0) / (s_e / sqrt(Sxx))
# Note that I needed to remove the name to compare these 2
all.equal(unname(manual_beta_1_hat_t), beta_1_hat_t)


# Verify p values

# We use pt function because we know that the t values follow a
# t-distribution
# Here the p value represents the point from which we have an area
# under the curve of 0.001  ??
# We can calculate the right side (tail) and multiple by 2
2 * pt(abs(beta_1_hat_t), df = nrow(cars) - 2, lower.tail = FALSE)
# which is reported by R in
summary(stop_dist_model)$coefficients["speed", "Pr(>|t|)"]
# We can also calculate from the left side, making sure we use
# the negative t value
2 * pt(-abs(beta_1_hat_t), df = nrow(cars) - 2)

# Seems like the t value represents the point from which we have
# an area of "p value"
# Then p value is the area under the curve
(area <- pt(-abs(beta_1_hat_t), df = nrow(cars) - 2))
(point <- qt(area, df = nrow(cars) - 2))

# Simulation with alpha 0.05

sim_slr <- function(x, beta_0 = 10, beta_1 = 5, sigma = 1) {
  n <- length(x)
  epsilon <- rnorm(n = n, mean = 0, sd = sigma)
  y <- beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

set.seed(1)
x <- seq(1, 20, length.out = 21)
# We simulate a model where the real beta_1 is 0, in this case
# we should Fail To Reject the null Hyp
#sim_data <- sim_slr(x, beta_0 = 2, beta_1 = 0, sigma = 1)
sim_data <- sim_slr(x, beta_0 = 2, beta_1 = 0.5, sigma = 7)

sim_fit <- lm(response ~ predictor, data = sim_data)
sim_fit$coefficients
summary(sim_fit)$sigma

# say that for a simulation beta_1_hat is 
# sim_fit$coefficients[2] = 0.70, the Null Hyp says:
# if we assume beta_1 = 0, how likely is that 0.70 appeared just by chance?

# Here we expect p value > alpha(0.05), so we Fail To Reject the Null Hyp H_0
# And seems to be ok for most cases since we know the model used 
# actually has beta_1 = 0
# but again, we might get a p value smaller than alpha
# and wrongly reject the Null Hyp

# When beta_1 = 0.5 we expect to reject the Null Hyp, in such case
# we expect p value < alpha. By chance we can also incorrectly FTR the Null Hyp
# if we run it enough times
summary(sim_fit)$coefficients["predictor", "Pr(>|t|)"]

plot(response ~ predictor, data = sim_data,
     xlab = "Predictor",
     ylab = "Response",
     main = "Simulation",
     pch = 20,
     cex = 2,
     col = "grey"
)
abline(sim_fit, lwd = 3, col = "darkorange")
# This line shows how it looks if beta_1 had been 0
abline(2, 0, lwd = 3, lty = 2, col = "dodgerblue")

#####

