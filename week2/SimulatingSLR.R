# simulation
# 1. The idea is to assume we know the parameters of the model.
# 2. We generate data based on that model
# 3. We calculate estimates doing normal SLR
# How far are the calculated parameters from the model parameters?


## model parameters
num_obs <- 21
beta_0 <- 5
beta_1 <- -2
sigma <- 3

## generate data

# first we get fixed values for x
x_vals <- seq(from = 0, to = 10, length.out = num_obs)

# then we get the errors, but the erros must follow a normal distribution
# we can use rnorm to get some random samples
set.seed(1)
(epsilon <- rnorm(n = num_obs, mean = 0, sd = sigma))

# then we calculate the corresponding y values
(y_vals <- beta_0 + beta_1 * x_vals + epsilon)


## fit model

sim_fit <- lm(y_vals ~ x_vals)
summary(sim_fit)

# The coefficients are not that far from the model parameters,
# so we can say LR is working fine
sim_fit$coefficients

plot(y_vals ~ x_vals)
abline(sim_fit)


## Let's make simulation repeatable 

sim_slr <- function(x, beta_0 = 10, beta_1 = 5, sigma = 1) {
  n <- length(x)
  epsilon <- rnorm(n = n, mean = 0, sd = sigma)
  y <- beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

# Use the function with the same parameters
set.seed(1)
sim_data <- sim_slr(x = x_vals, beta_0 = 5, beta_1 = -2, sigma = 3)
head(sim_data)

sim_fit <- lm(response ~ predictor, data = sim_data)
sim_fit$coefficients

plot(response ~ predictor, data = sim_data)
abline(sim_fit, lwd = 3, lty = 1, col = "darkorange")
abline(beta_0, beta_1, lwd = 3, lty = 2, col = "dodgerblue")

legend("topright", c("Estimate", "Truth"), 
       lty = c(1, 2),
       lwd = 3,
       col = c("darkorange", "dodgerblue"))

# Let's do another one we can run multiple times without seed

###### Important idea: 

# The parameters we estimate follow its own distribution based on
# the parameters we use for the model (the real ones)
#set.seed(1)
sim_data <- sim_slr(x = x_vals, beta_0 = 5, beta_1 = -2, sigma = 3)
sim_fit <- lm(response ~ predictor, data = sim_data)

i#sim_fit <- lm(response ~ predictor, data = sim_data)
sim_fit$coefficients

plot(response ~ predictor, 
     data = sim_data,
     ylim = c(-15, 10))
abline(sim_fit, lwd = 3, lty = 1, col = "darkorange")
abline(beta_0, beta_1, lwd = 3, lty = 2, col = "dodgerblue")

legend("topright", c("Estimate", "Truth"), 
       lty = c(1, 2),
       lwd = 3,
       col = c("darkorange", "dodgerblue"))

