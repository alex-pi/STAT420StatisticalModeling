# Q1

x <- 3

# The real mean when x is 3 would be
y <- 5 - 2*3 + 0  # when there is no error

pnorm(1, mean = y, sd = sqrt(3/4), lower.tail = FALSE)

# Q2

quiz_data
mod <- lm(y ~ x, data = quiz_data)
(max_lev_idx <- which.max(hatvalues(mod)))
#cooks.distance(mod)
cooks.distance(mod)[max_lev_idx]

# Q3

quiz_data
mod <- lm(y ~ x, data = quiz_data)
shapiro.test(mod$residuals)
shapiro.test(mod$residuals)$p.value

# Q4

quiz_data
mod <- lm(log(y) ~ x + I(x^2), data = quiz_data)
(pval <- shapiro.test(mod$residuals)$p.value)
pval < 0.05

# Q5


quiz_data
mod <- lm(log(y) ~ x + I(x^2), data = quiz_data)
sum((exp(mod$fitted.values) - quiz_data$y) ^ 2 ) / 1000000000 