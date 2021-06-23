# Question 1

# Having the same sd, if I keep the distance between
# q (first parameter) and the mean, the probability is the same.
# sd makes the same curve but the mean just "moves it" left
# or right
pnorm(-4, mean = 0, sd = 4)
pnorm(6, mean = 10, sd = 4)
pnorm(11, mean = 15, sd = 4)

# Question 2

y = 5
pnorm(3 , mean = y, sd = 4, lower.tail = FALSE)
pnorm(-2, mean = 0, sd = 4, lower.tail = FALSE)

# Question 3

y = 0
1-pnorm(3, sd = 4)

# Question 4

?faithful
ft <- faithful

ftm <- lm(ft$eruptions ~ ft$waiting)
ftm <- lm(eruptions ~ waiting, data = ft)

# Note that these give a different rounding,
# does it matter which one we get for the test?
ftm$coefficients
ftm$coefficients[1]

# Question 5

ftm$coef[2]

# Question 6

p1 <- predict(ftm, newdata = data.frame(
  waiting = 80
))

# Question 7

p2 <- predict(ftm, newdata = data.frame(
  waiting = 120
))

# Question 8

p1 < min(ft$eruptions) | p1 > max(ft$eruptions)
p2 < min(ft$eruptions) | p2 > max(ft$eruptions)

# Question 9

# RSS or SSE is the numerator for Residual standard error
sse <- sum((ft$eruptions - ftm$fitted.values) ^ 2)

summary(ftm)$sigma
s_e <- sqrt(sse / (nrow(ft) - 2))

# Question 10
summary(ftm)$r.squared

(sst <- sum((ft$eruptions - mean(ft$eruptions)) ^ 2))
var(ft$eruptions)
sst / (nrow(ft)-1)

sd(ft$eruptions)
sqrt(sst / (nrow(ft)-1))

(ssreg <- sum((ftm$fitted.values - mean(ft$eruptions)) ^ 2))

summary(ftm)$r.square
ssreg / sst
ssreg + sse

# Question 11

sd(ft$eruptions - ftm$fitted.values)

sqrt(sum((ft$eruptions - ftm$fitted.values) ^ 2) / (nrow(ft)-1))

# Question 12