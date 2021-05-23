# Title     : TODO
# Objective : TODO
# Created by: alejanpi
# Created on: 5/16/2021

fib <- c(1, 1, 2, 3, 5, 8, 13, 21)
ifelse(fib > 6, 'Foo', 'Bar')

## loops
x <- 11:15
for (i in 1:5) {
  x[i] <- x[i] * 2
}
x

silly_fun <- function(arg1, arg2, arg3 = 42) {
  a <- arg1 + arg2 - 3
  b <- a * arg3

  # last line is the return
  c(a, b, a + b, 0)
}

silly_fun(arg1 = 2, arg2 = 3, arg3 = 4)
