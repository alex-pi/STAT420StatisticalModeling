# Title     : TODO
# Objective : TODO
# Created by: alejanpi
# Created on: 5/18/2021

mpg <- ggplot2::mpg
View(mpg)
?mpg

## center
mean(mpg$cty)
sum(mpg$cty) / length(mpg$cty)

# median splits the data in 2 parts
median(mpg$cty)
sort(mpg$cty)[length(mpg$cty) / 2]

## spread
var(mpg$cty)

# how far away each data point is from the mean?
mpg$cty - mean(mpg$cty)
# then we want the mean od those deviations
mean(mpg$cty - mean(mpg$cty))
# but that is a very small number, so, in practice we
# get the mean of the squared deviations
# then we do a sqrt to go back the data units (miles/gallon)
sqrt(mean((mpg$cty - mean(mpg$cty)) ^ 2))
# that number is almost what we get with the sd function
sd(mpg$cty)

range(mpg$cty)
# this is the 1st quartile - 3rd quartile means: 5
# "it means where the middle 50% of the data is"
IQR(mpg$cty)

## summary

# 1st quartile means: 25% of the data is under value 14.00
# 3rd quartile means: 75% of the data is under value 19.00
summary(mpg$cty)

## let's set an outlier
mpg$cty[1] <- 500

# median and quartiles are not affected by the big outlier, so
# we say those measures are robust.
# On the other hand, mean is affected
summary(mpg$cty)
# sd also moves by a big margin
sd(mpg$cty)

## categorical data
table(mpg$drv)
table(mpg$drv) / nrow(mpg)

table(mpg$class)

