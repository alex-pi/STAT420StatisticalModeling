# Title     : TODO
# Objective : TODO
# Created by: alejanpi
# Created on: 5/17/2021

## matrices ##
x <- 1:9
# the matrix is fill by column by default
X <- matrix(x, nrow = 3, ncol = 3)
X

# transpose of X
Y <- matrix(x, nrow = 3, ncol = 3, byrow = TRUE)
Y

# matrix of zeros
Z <- matrix(0, 2, 4)
Z

# selection
# row 1 col 3
X[1, 3]
X[,1]
X[2,]
X[2, c(1, 3)]

# stacking vectors to create matrices
# seems like R finds a way to complete col3 just by repetition
# here col1 col2 col3 become names of the columns in the matrix
cbind(col1 = x, col2 = rev(x), col3 = 1)

X + Y
X * Y
X / Y

# matrix multiplication
X %*% Y

W <- matrix(c(9, 2, -3, 2, 4, -2, -3, -2, 16), 3, byrow=TRUE)
W

# get the inverse of a matrix
solve(W)
# verify by getting the identity matrix
(I <- solve(W) %*% W)
# diag is a funtion to return an identity matrix
diag(3)
# verify allowing smaill numerical differences
all.equal(I, diag(3))

# X is sigular so we get an error
solve(X)

# dimenions is a vector
dim(X)

rowSums(X)
colMeans(X)

## list ##
# each element of the list below is a vector of size 1
list(42, "Hello", TRUE)

ex_list <- list(
  a = 1:4,
  b = TRUE,
  c = "Hello!",
  d = function(arg = 2) {print("Hello World!")},
  e = diag(5)
)

ex_list

## subsetting lists

# returns the element named 'e' in the list
ex_list$e
# this returns a sublist
ex_list[1:2]
# this also returns a list of one element
# that one element is a vector of size 1
ex_list[1]
# dobule brackets will return the vector
ex_list[[1]]
# we can select the aliases to select elements of the list
ex_list[c("a", "b")]
# same idea, double bracket to get the element itself (not a sublist)
# here we also call the function
ex_list[["d"]]()
# or call the function like this
ex_list$d()

