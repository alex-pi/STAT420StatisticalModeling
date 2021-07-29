# Q1
#data_1
#data_2
mod_1 <- lm(y ~ x, data = data_1)
mod_2 <- lm(y ~ x, data = data_2)

par(mfrow = c(1,2))
qqnorm(resid(mod_1), main = "QQ-Plot, mod_1", col = "darkgrey")
qqline(resid(mod_1), lty = 2, lwd = 2, col = "dodgerblue")

qqnorm(resid(mod_2), main = "QQ-Plot, mod_2", col = "darkgrey")
qqline(resid(mod_2), lty = 2, lwd = 2, col = "dodgerblue")

# Q2
#data_1
#data_2
mod_1 <- lm(y ~ x, data = data_1)
mod_2 <- lm(y ~ x, data = data_2)

par(mfrow = c(1,2))
plot(mod_1$fitted.values, mod_1$residuals,  
     col = "gray", pch = 20,
     xlab = "Fitted", ylab = "Residuals",
     main = "mod_1")
abline(h = 0, col = "darkorange", lwd = 2)

plot(mod_2$fitted.values, mod_2$residuals,  
     col = "gray", pch = 20,
     xlab = "Fitted", ylab = "Residuals",
     main = "mod_1")
abline(h = 0, col = "darkorange", lwd = 2)

# Q3
x <- -3
(y <- 2 + 4*x)
pnorm(-12, mean = y, sd = 3)

# Q4
head(LifeCycleSavings)
srmod <- lm(sr ~ ., data = LifeCycleSavings)
stres <- abs(rstandard(srmod))
mean(stres < 2)
sum(stres < 2) / length(stres)

# Q5
head(LifeCycleSavings)
srmod <- lm(sr ~ ., data = LifeCycleSavings)
stres <- abs(rstandard(srmod))
which.max(stres)

# Q6
#head(LifeCycleSavings)
srmod <- lm(sr ~ ., data = LifeCycleSavings)
sum(hatvalues(srmod) > 2 * mean(hatvalues(srmod)))

# Q7
srmod <- lm(sr ~ ., data = LifeCycleSavings)
hatvalues(srmod)
(ml_idx <- which.max(hatvalues(srmod)))
LifeCycleSavings[ml_idx, ]

# Q8
srmod <- lm(sr ~ ., data = LifeCycleSavings)
summary(srmod)
srmod_cd <- cooks.distance(srmod)
sum(srmod_cd > 4 / length(srmod_cd))
srmod_fix <- lm(sr ~ ., data = LifeCycleSavings,
                subset = srmod_cd <= 4 / length(srmod_cd))
summary(srmod_fix)

sum(srmod_fix$coef)

# Q9 


# Q10
airquality = na.omit(airquality)
mod_quad <- lm(Ozone ~ Temp + I(Temp^2), data = airquality)
summary(mod_quad)
summary(mod_quad)$coef[3, "Pr(>|t|)"]

# Q11
airquality = na.omit(airquality)
mod_quad <- lm(Ozone ~ Temp + I(Temp^2), data = airquality)
mod_quar <- lm(Ozone ~ poly(Temp, degree = 4, raw = TRUE), data = airquality)
anova(mod_quad, mod_quar)
anova(mod_quad, mod_quar)[2, "Pr(>F)"]

# Q12
airquality = na.omit(airquality)
mod_quar <- lm(Ozone ~ poly(Temp, degree = 4, raw = TRUE), data = airquality)
shapiro.test(mod_quar$residuals)
shapiro.test(mod_quar$residuals)$p.value < 0.01

# Q13
airquality = na.omit(airquality)
mod_log <- lm(log(Ozone) ~ Temp, data = airquality)
shapiro.test(mod_log$residuals)
shapiro.test(mod_log$residuals)$p.value < 0.01

# Q14
airquality = na.omit(airquality)
mod_log <- lm(log(Ozone) ~ Temp, data = airquality)
(interv <- predict(mod_log, level = 0.9, interval = "prediction",
                   newdata = data.frame(Temp = 84)))

exp(interv)
exp(interv[1, "upr"])

# Q15
airquality = na.omit(airquality)
mod_log <- lm(log(Ozone) ~ Temp, data = airquality)
lt <- fitted(mod_log) < 3.5
gt <- fitted(mod_log) > 3.5

(var_lt <- var(resid(mod_log)[lt]))
(var_gt <- var(resid(mod_log)[gt]))

var_lt / var_gt

#(var_lt <- var(rstandard(mod_log)[lt]))
#(var_gt <- var(rstandard(mod_log)[gt]))

#var_lt / var_gt
