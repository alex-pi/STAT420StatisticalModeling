
x <- 1:100
length(x)

odd <- seq(1, 100, 2)
even <- seq(2, 100, 2)
y = x
y


y[odd] <- x[odd] + 5
y[even] <- x[even] - 10
sd(y)

y
x


quiz_list = list(
  x = c(1, 2),
  y = "Hello Quiz Taker",
  z = "z"
)

quiz_list[3]

quiz_list[[3]]

quiz_list$z

library(MASS)
Melanoma <- MASS::Melanoma
hist(Melanoma$age)

hist(rchisq(1500, df = 5), breaks = 20)
