# Title     : Vectors
# Objective : Basic Vector concepts
# Created by: alejanpi
# Created on: 5/16/2021

# data types

42.5
is.numeric(42.5)
is.double(42.5)

# vectors

2
x <- c(1, 3, 5, 7, 8, 9)
# the () make the expression to be shown in the output
(y <- 1:100)

c(42, 'Statistics', TRUE)
c(42, TRUE, FALSE)

z <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
z

seq(from = 1.5, to = 4.2, by = 0.1)
seq(1, 9, 2)

rep("A", times = 10)
rep(x, 3)

# subsetting
# R vectors start on 1 (not 0)
x[1]
# subset or slice
x[1:3]
# masking
x[z]
# selecting with a vectors of indexes
x[c(1, 3, 4)]

c(1:10, rep(42, times = 10))
c(1:10, rep(42, times = 10))[seq(2, 20, by = 2)]
# R completes with NA when indexes are out of bound
c(1:10, rep(42, times = 10))[seq(2, 30, by = 2)]

x
x[x > 3]
as.numeric(x > 3)

# It counts how many elements of x are > 3
sum(x > 3)
# It returns the indexes as opposed to x[x > 3]
which(x > 3)
# so, below line is the same as x[x > 3]
x[which(x > 3)]
# sum all elements x that are > 3
sum(x[which(x > 3)])

max(x)

# 2 ways to get the index of the max
which(x == max(x))
which.max(x)

# vectorization of operations

# R does the following for x + 2
# 1) since x has elements and (2) is a vector of length 1
#    R repeats the 2 as many times to match the lenght of x
# 2) Once the vetors are of the same size, it does an
#    element wise sum, which is the same as adding 2 to
#    each element of x
x + 2
# so below is the same but more verbose
x + rep(2, 6)

# the same idea applies to this logical operation
x > 3

y <- 1:100
x
y
# x is repeated to match y, but R gives a warning
# length(x) is not a multiple of length(y)
x + y
length(x)
length(y)
length(y) / length(x)
# if we substract y we can see x repeats entirely until
# the last part where it is truncated
(x + y) - y

# Hence, the following has no warning
w <- 1:60
x + w

# predicates for comparing vectors
all(x + w == rep(x, 10) + w)
identical(x + w, rep(x, 10) + w)

# this one allows for small errors due to numerical computations
?all.equal