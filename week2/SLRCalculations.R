

plot(dist ~ speed, data = cars,
     xlab = "Speed",
     ylab = "Stopping distance",
     pch = 20,
     cex = 2,
     col = "grey"
)

x <- cars$speed
y <- cars$dist

Sxy <- sum((x - mean(x)) * (y - mean(y)))
Sxx <- sum((x - mean(x)) ^ 2)
Syy <- sum((y - mean(y)) ^ 2)

c(Sxy, Sxx, Syy)

(beta_1_hat <- Sxy / Sxx)
(beta_0_hat <- mean(y) - beta_1_hat * mean(x))

abline(beta_0_hat, beta_1_hat, lwd = 3, col = "darkorange")

# predictions

# remove duplicates
unique(cars$speed)

# is 8 in the data set?

8 %in% cars$speed
beta_0_hat + beta_1_hat * 8

# Interpolation means to predict a value in the range
# of the data set but is NOT in the dataset
min(cars$speed) < 21 & 21 < max(cars$speed)
beta_0_hat + beta_1_hat * 21


# Extrapolation, predict using a point out of the range of
# the observed data
r <- range(cars$speed)
r[1] < 50 & r[2] > 50
beta_0_hat + beta_1_hat * 50


# residuals

which(cars$speed == 8)
cars[5, ]
cars[which(cars$speed == 8), ]

# actual value minus predicted is the residual
16 - (beta_0_hat + beta_1_hat * 8)

# Vectorizing all

(y_hat <- beta_0_hat + beta_1_hat * x)
(e <- y - y_hat)
n <- length(e)
(s2_e <- sum(e ^ 2) / (n - 2))
(s_e <- sqrt(s2_e))

# This is a line because we are predicting with the original data
plot(x, y_hat)

# Variance decomposition

(SST <- sum((y - mean(y)) ^ 2))
(SSReg  <- sum((y_hat - mean(y)) ^ 2))
(SSE <- sum((y - y_hat) ^ 2))

# This is how we estimate the variance
SSE / (n - 2)

# coefficient of determination

# This is understood as 65% of the variation in the stopping distance is
# explained by the speed !!! Clearly there other factors for the rest (35%) 
# of the variance
(R2 <- SSReg / SST)
