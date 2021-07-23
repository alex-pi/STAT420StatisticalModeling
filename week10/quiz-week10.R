# Question 1

(Y <- -3 -1 + 1 + 0.75)

1 - (1 / (1 + exp(-Y)))

# Question 2

mod2 <- glm(am ~ mpg + hp + qsec, data = mtcars, family = binomial)

mod2$coefficients[4]

# Question 3

mod2$coefficients["mpg"]

# Question 4

predict(mod2, newdata = data.frame(
  mpg = 19,
  hp = 150,
  qsec = 19
), type = "link")


# Question 5

predict(mod2, newdata = data.frame(
  mpg = 22,
  hp = 123,
  qsec = 18
), type = "response")

# Question 6

null_mod <- glm(am ~ 1, data = mtcars, family = binomial) 

anov <- anova(null_mod, mod2, test = "LRT")

anov$Deviance[2]

(devian <- -2 * as.numeric(logLik(null_mod) - logLik(mod2)))

summary(mod2)

# Question 7

summary(mod2)$coef
summary(mod2)$coef[3, "Pr(>|z|)"]

# Question 8

trn <- MASS::Pima.tr
tes <- MASS::Pima.te
head(trn)

mod8 <- glm(type ~ (glu + ped)^2 + I(glu^2) + + I(ped^2), 
            data = trn, family = binomial)

summary(mod8)

mod8$coef[5]

# Question 9

mean(predict(mod8, newdata = tes, type = "response") > 0.80)

# Question 10

mod10 <- glm(type ~ ., data = trn, family = binomial)

mod10_small <- step(mod10, direction = "backward", trace = 0)

(anov <- anova(mod10_small, mod10, test = "LRT"))

# Question 11

mod11 <- glm(type ~ .^2, data = trn, family = binomial)

mod11_small <- step(mod11, direction = "backward")

mod11_small

mod11_small$deviance

# Question 12

# fit the models here

mod8_small <- step(mod8, direction = "backward", 
                   k = log(nrow(trn)), trace = 0)
library(boot)
set.seed(42)
# get cross-validated results for the polynomial model here
cv.glm(trn, mod8, K = 5)$delta[1]
set.seed(42)
# get cross-validated results for the additive model here
cv.glm(trn, mod10, K = 5)$delta[1]
set.seed(42)
# get cross-validated results for the model selected from additive model here
cv.glm(trn, mod10_small, K = 5)$delta[1]
set.seed(42)
# get cross-validated results for the interaction model here
cv.glm(trn, mod11, K = 5)$delta[1]
set.seed(42)
# get cross-validated results for the model selected from interaction model here
cv.glm(trn, mod11_small, K = 5)$delta[1]

# Question 13 XXX

mod10_small2 <- step(mod10, direction = "backward", 
                   k = log(nrow(trn)), trace = 0)

#cv.glm(tes, mod10_small2, K = 5)$delta[1]

# When they say:
# create a classifier that seeks to minimize 
# the misclassification rate. 
# Report the misclassification rate or this classifier 
# in the test dataest.

# I just compared to the test data without using step(bic)

# worng pred <- ifelse(predict(mod10_small2, tes, type = "response") > 0.5, 
#               "Yes", "No")

pred <- ifelse(predict(mod10, tes, type = "response") > 0.5, 
                "Yes", "No")

mean(pred != tes$type)

# Question 14 XXX

(confusion <- table(predicted = pred, actual = tes$type))

confusion[2, 2] / sum(confusion[, 2])

# Questio 15

pred3 <- ifelse(predict(mod10, tes, type = "response") > 0.3, 
               "Yes", "No")

(confusion3 <- table(predicted = pred3, actual = tes$type))

confusion3[2, 2] / sum(confusion3[, 2])












