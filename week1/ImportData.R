# Title     : TODO
# Objective : TODO
# Created by: alejanpi
# Created on: 5/17/2021

example_data <- data.frame(
  x = seq(1, 9, 2),
  y = c(rep("hello", 9), "Bye"),
  z = rep(c(TRUE, FALSE), 5)
)

example_data

library(readr)
ex_from_csv <- read_csv("week1/example-data.csv") # faster, tibble
ex_from_csv$y

# why is this one not comming up as a Factor (it comes as character)
example_data$y

read.csv("week1/example-data.csv") # slower, data.frame
