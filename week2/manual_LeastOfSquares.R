
## Least squares method
ft <- faithful
ftm <- lm(ft$eruptions ~ ft$waiting)

x <- ft$waiting
x_mean <- mean(ft$waiting)
Sxx <- sum((x - x_mean)^2)

y <- ft$eruptions
y_mean <- mean(ft$eruptions)

Sxy <- sum((x - x_mean)*(y - y_mean))

(beta_1_hat <- Sxy / Sxx)
(beta_0_hat <- y_mean - (beta_1_hat * x_mean))

ftm$coef



