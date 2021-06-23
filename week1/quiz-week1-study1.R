# Question 1

mel <- MASS::Melanoma
?MASS::Melanoma
nrow(mel)
sum(mel$status == 1)

# Question 2

mean(mel[mel$status == 2, ]$age)

# Question 3

mam <- MASS::mammals
?MASS::mammals
nrow(mam)
idx <- which.max(mam$brain / mam$body)
row.names(mam[idx, ])

# Question 4

?iris
boxplot(rnorm(100))
head(iris)
boxplot(iris[, 1:4])

sd(iris[, 1:2])

# Calculate the sd by column
apply(iris[, 1:4], 2, sd)

# Question 5

min(z[[1]])

min(z[[2]])

mean(z[[3]])


# Question 6

?airquality


# Question 7
head(airquality)
air <- airquality

mean(air[air$Month == 5, ]$Wind)

# Question 8
mean(air$Ozone, na.rm = TRUE)

plot(air$Wind, air$Temp)


#Question 10

set.seed(1337)
x = rnorm(10000)

sum(abs(x) > 2) / length(x)

sum(x < 0)


# Question 11

f <- function(input = 42) {
  input[input < 0] <- 0
  input
}

set.seed(42)
x = rnorm(100, mean = 0, sd = 10)
mean(f(input = x)) - f()

# Question 12

x0 <- rep(1, times = 30)
x1 <- seq(1, 30) ^ 2
#(1:30) ^ 2

set.seed(42)
y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)
mean(y)

# Question 13

X <- cbind(x0, x1)

sum(X[17, ], X[19, ])

# Question 14

library(MASS)
x0 <- rep(1, 30)
x1 <- (1:30) ^ 2
set.seed(42)
y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

sum(beta_hat)

# Question 15

y_hat <- X %*% beta_hat

sum((y - y_hat)^2)













