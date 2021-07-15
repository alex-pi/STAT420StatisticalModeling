# Question 1

?mtcars

add_mod <- lm(mpg ~ ., data = mtcars)

max(vif(add_mod))

# Question 2

names(summary(add_mod))
summary(add_mod)$adj.r.squared

# Question 3

calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
calc_loocv_rmse(add_mod)

# Question 4

q4_mod <- step(add_mod, direction = "backward", trace = 0)


# Question 5

calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
calc_loocv_rmse(q4_mod)


# Question 6

max(vif(q4_mod))


#Question 7

vif(add_mod) > 5

summary(add_mod)$adj.r.squared

summary(q4_mod)$adj.r.squared

summary(add_mod)$r.squared

summary(q4_mod)$r.squared

sqrt(mean(resid(add_mod) ^ 2))

sqrt(mean(resid(q4_mod) ^ 2))

calc_loocv_rmse(add_mod)

calc_loocv_rmse(q4_mod)

# Question 8

start_mod <- lm(mpg ~ 1, data = mtcars)

q8_mod <- step(lm(mpg ~ 1, data = mtcars), 
               direction = "forward",
               scope = mpg ~ cyl + disp + hp 
                        + drat + wt + qsec + vs + am + gear + carb,
               k = log(nrow(mtcars)))

cnames <- (dimnames(mtcars)[[2]])[-1]

fmla <- as.formula(paste(" ~ ",paste(cnames,collapse="+")))

q8_mod <- step(lm(mpg ~ cyl, data = mtcars), 
               direction = "forward",
               scope = list(upper=fmla,lower=~1) ,
               k = log(nrow(mtcars)))

q8_mod$coefficients

step(add_mod, direction = "backward")

all_mod_sum = summary(regsubsets(mpg ~ ., data = mtcars))

all_mod <- regsubsets(mpg ~ ., data = mtcars)

all_mod$which

extractAIC(all_mod)


# Question 9

calc_loocv_rmse(q8_mod)

# Question 10

?LifeCycleSavings

sr_add_mod <- lm(sr ~ . - ddpi, data = LifeCycleSavings)
ddpi_add_mod <- lm(ddpi ~ . - sr, data = LifeCycleSavings)

cor(ddpi_add_mod$residuals, sr_add_mod$residuals)

# Question 11

inter_mod <- lm(sr ~ . ^ 2, data = LifeCycleSavings)

summary(inter_mod)$adj.r.squared

# Question 12

q12_mod <- step(inter_mod, direction = "backward",
                k = log(nrow(LifeCycleSavings)),
                trace = 0)

coef(q12_mod)

# Question 13

q13_mod <- step(inter_mod, direction = "backward", trace = 0)

coef(q13_mod)

# Question 14

calc_loocv_rmse(inter_mod)
calc_loocv_rmse(q13_mod)
calc_loocv_rmse(lm(sr ~ ., data = LifeCycleSavings))

# Question 15

summary(inter_mod)$adj.r.squared
summary(q13_mod)$adj.r.squared
summary(lm(sr ~ ., data = LifeCycleSavings))$adj.r.squared



