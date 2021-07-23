# Question 1

x1 <- 1
x2 <- 0

(Y = 2 + (-1)*x1 + (-1)*x2)

boot::inv.logit(Y)

1/(1 + exp(-Y))

pbinom(Y, size = 1, prob = 0.5)

# Question 2

colnames(quiz_data)
mod2 <- glm(y ~ ., data = quiz_data, family = "binomial")
mod2$coef
mod2$coef[3]

# Question 3

mod2 <- glm(y ~ ., data = quiz_data, family = "binomial")
summary(mod2)$coef
summary(mod2)$coef[4, "Pr(>|z|)"]

# Question 4

mod2 <- glm(y ~ ., data = quiz_data, family = "binomial")
mod2_small <- step(mod2, direction = "backward", 
                   k = log(nrow(quiz_data)), trace = 0)

(anov <- anova(mod2_small, mod2, test = "LRT"))
anov[2, "Pr(>Chi)"]

devian <- -2 * as.numeric(logLik(mod2_small) - logLik(mod2))

pchisq(devian, df = 5, lower.tail = FALSE)

# Question 5

#quiz_data
mod2 <- glm(y ~ ., data = quiz_data, family = binomial)

#why do I need to use BIC in here?
mod2_small <- step(mod2, direction = "backward", 
                   k = log(nrow(quiz_data)), trace = 0)
library(boot)
set.seed(1)
cv.glm(quiz_data, mod2_small, K = 5)$delta[1]

