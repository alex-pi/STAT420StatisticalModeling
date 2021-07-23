

# fit model
stop_dist_model <- lm(dist ~ speed, data = cars)

# plot model and data
plot(dist ~ speed, data = cars,
     xlab = "Speed",
     ylab = "Stopping distance.",
     main = "Stopping distance vs Speed",
     pch = 20,
     cex = 2,
     col = "grey"
)
abline(stop_dist_model, lwd = 3, col = "darkorange")

confi_interval <- confint(stop_dist_model, level = 0.99)
confi_interval[2, ]

# We can obtain it for each coefficient individually
confint(stop_dist_model, parm = "(Intercept)", level = 0.99)
confint(stop_dist_model, parm = "speed", level = 0.99)

# Explaining intervals
#                 0.5 %    99.5 %
#  (Intercept) -35.706610 0.5484205
#  speed        2.817919 5.0468988

# We asked for a 99% of confidence, so the the remaining
# 1% is split to the left and right of that margin.
# We read this as:
# We are 99% confident that the true Beta_1 is somewhere between
# 2.817919 5.0468988

# Now, more accurately, Beta_1 represents the true change in mean
# stopping distance (Y) for an increase in speed (x) of 1 mph

# The smaller the confidence level is the smaller the
# confidence interval gets.

confint(stop_dist_model, parm = "speed", level = 0.90)


# The standard errors from the lecture (SE[Beta_hat_1])
summary(stop_dist_model)
names(summary(stop_dist_model))

summary(stop_dist_model)$coefficients

# point estimate for beta_1_hat
(beta_1_hat <- stop_dist_model$coefficients[2])

# standard error SE for beta_1_hat
(beta_1_hat_se <- summary(stop_dist_model)$coefficients[2, 2])

# Calculate it manually to verify
Sxx <- sum((cars$speed - mean(cars$speed)) ^ 2)
# Do not confuse s_e with Standard Error (SE) !!!!

# s_e is the residuals Standard Error
sqrt(sum((cars$dist - stop_dist_model$fitted.values)^2) 
     / (nrow(cars) - 2))
(s_e <- summary(stop_dist_model)$sigma)

# Then the Standard Error for beta_1_hat would be
s_e / sqrt(Sxx) # Same as summary(stop_dist_model)$coefficients[2, 2]

# Then for the margin of confidence we also need the critical value
# alpha here is the confidence level we established

# We want a value with (1 - 0.99) / 2 of "Area" to the right of it
(1 - 0.99) / 2 # 0.005
# But for R that would be from the left, so we can do it in 2 ways
# also we need n - 2 degrees of freedom
(crit <- qt(0.995, df = nrow(cars) - 2))
(crit <- qt(0.005, df = nrow(cars) - 2, lower.tail = FALSE))

# Then we can put the margin together
c(beta_1_hat - crit * beta_1_hat_se, beta_1_hat + crit * beta_1_hat_se)
# Which should be the same as the one from R
confi_interval

# Notes on qt and pt
# Both are functions for t distributions
# qt gives the point from which the Area under the curve is 0.995
(point <- qt(0.995, df = nrow(cars) - 2))
# pt would be the opposite, the area under the curve to the left of point
pt(point, df = nrow(cars) - 2)

## Confidence Interval for the mean response ##
new_speeds <- data.frame(speed = c(5,21))
predict(stop_dist_model, newdata = new_speeds)

predict(stop_dist_model, newdata = new_speeds,
        interval = c("confidence"), level = 0.99)

# Here we are saying that for x = 5 we are 99% confident that the
# mean response (Y) is somewhere between -10.89309 15.05898

# Personally I understand that if we take more samples from real life,
# we are 99% confident that for x = 5 the mean value of Y 
# is somewhere between -10.89309 15.05898

## Prediction Interval for new observations

# The interval is larger then the one for CI, that is because we are saying:
# we are 99% confident that new Y observations will fall in this interval for
# x = 5 
predict(stop_dist_model, newdata = new_speeds,
        interval = c("prediction"), level = 0.99)




## Next, a graph that shows lines for CI and PI intervals

speed_grid <- seq(min(cars$speed), max(cars$speed), 0.01)

dist_ci_bad <- predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid),
                       interval = c("confidence"), level = 0.9)

dist_pi_bad <- predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid),
                       interval = c("prediction"), level = 0.99)


plot(dist ~ speed, data = cars,
     xlab = "Speed",
     ylab = "Stopping distance.",
     main = "Stopping distance vs Speed",
     pch = 20,
     cex = 2,
     col = "grey",
     ylim = c(min(dist_pi_bad), max(dist_pi_bad))
)
abline(stop_dist_model, lwd = 3, col = "darkorange")

lines(speed_grid, dist_ci_bad[, "lwr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_ci_bad[, "upr"], col = "dodgerblue", lwd = 3, lty = 2)

# Note that the Prediction Band contain 99% of the observed data points
lines(speed_grid, dist_pi_bad[, "lwr"], col = "red", lwd = 3, lty = 3)
lines(speed_grid, dist_pi_bad[, "upr"], col = "red", lwd = 3, lty = 3)

points(mean(cars$speed), mean(cars$dist), pch = "+", cex = 3)

