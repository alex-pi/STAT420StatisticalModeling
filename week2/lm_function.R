# slr with lm()

stop_dist_model = lm(dist ~ speed, data = cars)
stop_dist_model

# c(beta_0_hat, beta_1_hat)

plot(dist ~ speed, data = cars,
     xlab = "Speed",
     ylab = "Stopping distance",
     pch = 20,
     cex = 2,
     col = "grey"
     )

abline(stop_dist_model, lwd = 3, col = "darkorange")

names(stop_dist_model)

stop_dist_model$coefficients
stop_dist_model$fitted.values
stop_dist_model$residuals

# Functions to extract the same 3 fields above
coef(stop_dist_model)
fitted(stop_dist_model)
resid(stop_dist_model)

# Check that diff between data and fitted is equal to the residuals
all.equal(cars$dist - stop_dist_model$fitted.values, stop_dist_model$residuals)


# summary()

summary(stop_dist_model)
names(summary(stop_dist_model))


summary(stop_dist_model)$r.squared
summary(stop_dist_model)$sigma

# predict()

predict(stop_dist_model, newdata = data.frame(speed = 8))

predict(stop_dist_model, newdata = data.frame(speed = c(8, 21, 50)))

# This are the same as the fitted values
predict(stop_dist_model, newdata = cars)
