set.seed(42)
sample_size <- 100
x <- seq(-1, 1, length = sample_size)
 

beta_0 <- 3
beta_1 <- 6
sigma <- 2

# These are the variances used for beta1 and beta2 distributions
var_beta_1_hat <- sigma ^ 2 / Sxx
var_beta_0_hat <- sigma ^ 2 * (1/sample_size + mean(x) ^ 2 / Sxx)

num_samples <- 10000
beta_0_hats <- rep(0, num_samples)
beta_1_hats <- rep(0, num_samples)

for(i in 1:num_samples) {
  eps <- rnorm(sample_size, mean = 0, sd = sigma)
  y <- beta_0 + beta_1 * x + eps
  
  sim_model <- lm(y ~ x)
  
  beta_0_hats[i] <- sim_model$coefficients[1]
  beta_1_hats[i] <- sim_model$coefficients[2]
}

# empirical mean vs true mean
mean(beta_1_hats)
beta_1
mean(beta_0_hats)
beta_0

# empirical variance vs true variance
var(beta_1_hats)
var_beta_1_hat
var(beta_0_hats)
var_beta_0_hat

hist(beta_1_hats, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_1, sd = sqrt(var_beta_1_hat)),
      col = "darkorange", add = TRUE, lwd = 3)

# For instance we can get the probability for Beta_1_hat to be > than 6.5
pnorm(6.5, mean = beta_1, sd = sqrt(var_beta_1_hat))

pnorm(6.5, mean = beta_1, sd = sqrt(var_beta_1_hat), lower.tail = FALSE)

1 - pnorm(6.5, mean = beta_1, sd = sqrt(var_beta_1_hat))

hist(beta_0_hats, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[0]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_0, sd = sqrt(var_beta_0_hat)),
      col = "darkorange", add = TRUE, lwd = 3)

par(mar = c(5, 5, 1, 1)) # adjusted plot margins, otherwise the "hat" does not display
plot(cumsum(beta_1_hats) / (1:length(beta_1_hats)), type = "l", ylim = c(5.95, 6.05),
     xlab = "Number of Simulations",
     ylab = expression("Empirical Mean of " ~ hat(beta)[1]),
     col  = "dodgerblue")
abline(h = 6, col = "darkorange", lwd = 2)

par(mar = c(5, 5, 1, 1)) # adjusted plot margins, otherwise the "hat" does not display
plot(cumsum(beta_0_hats) / (1:length(beta_0_hats)), type = "l", ylim = c(2.95, 3.05),
     xlab = "Number of Simulations",
     ylab = expression("Empirical Mean of " ~ hat(beta)[0]),
     col  = "dodgerblue")
abline(h = 3, col = "darkorange", lwd = 2)