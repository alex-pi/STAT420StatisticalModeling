set.seed(1337)
n <- 100
p <- 3
beta_0 <- 5
beta_1 <- -2
beta_2 <- 6
sigma <- 4

x0 <- rep(1, n)
x1 <- sample(seq(1, 10, length = n))
x2 <- sample(seq(1, 10, length = n))
X <- cbind(x0, x1, x2)
C <- solve(t(X) %*% X)

C[3, 3]
C[2 + 1, 2 + 1]
# Variance
sigma ^ 2 * C[2 + 1, 2 + 1]
sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])

y <- rep(0, n)
num_sims <- 10000

### ALWAYS PREALLOCATE IN R INSTEAD OF MAKING VECTORS GROW EACH TIME!!!!
beta_hat_2 <- rep(0, num_sims)

for(i in 1:num_sims) {
  eps <- rnorm(n, mean = 0, sd = sigma)
  y <- beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit <- lm(y ~ x1 + x2)
  beta_hat_2[i] <- coef(fit)[3]
}

mean(beta_hat_2)
var(beta_hat_2) # close to sigma ^ 2 * C[2 + 1, 2 + 1]

hist(beta_hat_2, probability = TRUE, breaks = 20,
     xlab = expression(hat(beta)[2]), main = "", border = "dodgerblue")

## I just notice here that dnorm is an expression,
## does not look like is executing and then the result is pass to curve...
## rather seems like curve evaluate the whole dnorm() call as an expression
curve(dnorm(x, mean = beta_2, sd = sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

# Instead of the loop we can use replicate function
# which seems to be a sort of function programming mechanism...


sim_beta_hat_2 <- function(){
  eps <- rnorm(n, mean = 0, sd = sigma)
  y <- beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit <- lm(y ~ x1 + x2)
  coef(fit)[3]
}
beta_hat_2_alt <- replicate(n = num_sims, sim_beta_hat_2())

system.time(
  for(i in 1:num_sims) {
    eps <- rnorm(n, mean = 0, sd = sigma)
    y <- beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
    fit <- lm(y ~ x1 + x2)
    beta_hat_2[i] <- coef(fit)[3]
  }
)

system.time(
  beta_hat_2_alt <- replicate(n = num_sims, sim_beta_hat_2())
)

# Note that if I use equals instead of <- to assign the result
# then I need to tell R that it is a whole expression and not
# a parameter being passed to the time function!!!

system.time(
  {beta_hat_2_alt = replicate(n = num_sims, sim_beta_hat_2())}
)
