# Title     : TODO
# Objective : TODO
# Created by: alejanpi
# Created on: 5/17/2021

## A data frame combines lists and vectors

example_data <- data.frame(
  x = seq(1, 9, 2),
  y = c(rep("Hello", 9), "Bye"),
  z = rep(c(TRUE, FALSE), 5)
)
# Has a nice structure with column names and ids for the rows
# vectors in the data frame should be of the same length
example_data

all.equal(length(example_data$x),
          length(example_data$x),
          length(example_data$x))

# to show the structure of the data frame
str(example_data)

nrow(example_data)
ncol(example_data)
dim(example_data)

example_data$y

## load from package

(Galton <- mosaicData::Galton)

View(Galton)
head(Galton, n = 10)
str(Galton)

Galton$sex[10:15]
levels(Galton$sex)

## we access it like a matrix
# the third variable of the seventh row (observation)
Galton[7, 3]
# all rows but only second variable
Galton[, 2]
# select some variables (columns) for 10 observations
Galton[1:10,c(2, 3, 5)]

## like a list
# this returns a vector
Galton$father
Galton[['father']]
# while this returns a data frame
Galton[2]
Galton['father']

## Here we start using them similar to pandas DF
Galton[Galton$sex == 'F', 'height']
# OR
Galton[Galton$sex == 'F', ]$height

head(subset(Galton, subset = height > 73), n = 10)

## data.frame vs tibble
library(tibble)
(Galton2 <- as_tibble(Galton))

Galton2['height'] # tibble
Galton2$height    # vector
Galton2[, 5]      # tibble
Galton2[1, 5]     # tibble

Galton['height'] # data frame
Galton$height    # vector
Galton[, 5]      # vector
Galton[1, 5]     # vector
