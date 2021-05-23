# Question 1

library(MASS)
Melanoma <- MASS::Melanoma
str(Melanoma)

?Melanoma
sum(Melanoma$status == 1)

hist(Melanoma$status)

# Question 2
Melanoma$status == 1

mean(Melanoma[Melanoma$status == 2, ]$age)

mean(Melanoma$status == 1)

# Question 3

mammals <- MASS::mammals
View(mammals)
mammals
str(mammals)

ratio <- mammals$brain / mammals$body
ratio
max(ratio)
which.max(ratio)
rownames(mammals)[which.max(ratio)]
rownames(mammals)[which.min(ratio)]

4.00 / 0.101

#Question 4
library(datasets)
iris <- datasets::iris
?iris
str(iris)

boxplot(iris[1:4])
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

mean(iris$Petal.Length)

# Question 5
mean(c(1, 2, 3))
#sum(min(z[[1]]), max(z[[2]]), mean(z[[3]]))

# Question 6
?airquality
str(airquality)

# Question 7
airquality$Month == 5
mean(airquality[airquality$Month == 5, 'Wind'])

# Question 8
?mean
mean(airquality$Ozone, na.rm = TRUE)

# Question 9
plot(Wind ~ Temp, data = airquality)

# Question 10
set.seed(1337)
x = rnorm(10000)
x
abs(x)
sum(abs(x) > 2)
sum(abs(x) <= 2)
length(x)
sum(abs(x) > 2) / length(x)

# Question 11
# write your function here
f <- function(input = 42) {
  input[input < 0] = 0
  input
}

set.seed(42)
x = rnorm(100, mean = 0, sd = 10)
x
f(input = x)
x
f()
mean(f(input = x)) - f()

# Question 12

x0 <- rep(1, 30)
x1 <- (1:30) ^ 2
set.seed(42)
y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)
mean(y)

# Question 13
X <- cbind(x0, x1)
X
sum(X[17,] + X[19, ])

# Question 14
library(MASS)
t(X)
?t
B_hat1 <- (t(X) %*% X) 

A <- solve(crossprod(X))
A

B <- crossprod(X, y)
B

B_hat <- crossprod(A, B)

B_hat
sum(B_hat)

ginv(crossprod(X))

# Question 15
y_hat <- X %*% B_hat

sum((y - y_hat) ^ 2)
