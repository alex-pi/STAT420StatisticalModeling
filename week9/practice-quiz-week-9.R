# Question 1

head(quiz_data)
minus_x1_mod <- lm(y ~ . - x1, data = quiz_data)
x1_mod <- lm(x1 ~ . - y, data = quiz_data)
cor(x1_mod$residuals, minus_x1_mod$residuals)

# Question 2

head(quiz_data)
x5_mod <- lm(x5 ~ . - y, data = quiz_data)
r2j <- summary(x5_mod)$r.squared
1 / (1 - r2j)

# Question 3

#quiz_data
add_mod <- lm(y ~ ., data = quiz_data)
add_3_mod <- lm(y ~ x1 + x2 + x3, data = quiz_data)
extractAIC(add_mod)
extractAIC(add_3_mod)

sum(add_mod$residuals ^ 2)

# Question 4

#quiz_data
add_3_mod <- lm(y ~ x1 + x2 + x4, data = quiz_data)
add_4_mod <- lm(y ~ x3 + x4 + x5 + x6, data = quiz_data)
#summary(add_3_mod)
summary(add_3_mod)$adj.r.squared
summary(add_4_mod)$adj.r.squared

# Question 5

#quiz_data
mod_start <- lm(y ~ ., data = quiz_data)
selected <- step(mod_start, direction = "backward")
calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
calc_loocv_rmse(selected)