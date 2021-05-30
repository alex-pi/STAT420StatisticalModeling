# Question 1
#Consider a random variable XX that has a normal distribution with a mean of 5 and a variance of 9. 
#Calculate P[X > 4]P[X>4].

pnorm(4, mean = 5, sd = 3, lower.tail = FALSE)


# Question 2

# Consider the simple linear regression model Y = -3 + 2.5x + \epsilon

# where \epsilon \sim N(0, \sigma^2 = 4)

# What is the expected value of Y given that x = 5? That is, what is \text{E}[Y \mid X = 5]?
  
?rnorm
# One idea is to consider the mean of the Y_i distribution as -3 + 2.5 * 5 
(ep <- rnorm(1, mean = 9.5, sd = 2))
-3 + 2.5 * 5 + ep

# But seems like the "expected" value is the mean itself, as in, we expect
# our prediction to be exactly on the line, without extra error/noise
-3 + 2.5 * 5 

# Question 3

# According to the book, the variance remains the same for any x_i
# What changes is the mean of that distribution
# Y_i has a Normal distribution for each X_i

# Question 4
?trees
sim_fit <- lm(Girth ~ Height, data = trees)
sim_fit$coefficients['Height']
sim_data$rank
summary(sim_fit)
names(sim_fit)

plot(Girth ~ Height, data = trees)
abline(sim_fit, lwd = 3, lty = 1, col = "darkorange")

# Question 5
y <- trees$Girth
(SST <- sum((y - mean(y)) ^ 2))
(y_hat <- predict(sim_fit, newdata = trees))
(SSReg  <- sum((y_hat - mean(y)) ^ 2))
SSReg / SST

# Another way to get the R^2

names(summary(sim_fit))
summary(sim_fit)$r.squared
