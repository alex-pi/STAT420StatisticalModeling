library(leaps)

check <- function(model) {
        print(get_loocv_rmse(model))
        print(get_adj_r2(model))
        print(get_bp_decision(model, alpha = 0.01))
        print(get_sw_decision(model, alpha = 0.01))
        print(get_num_params(model))
}

add_balance_mod <- lm(Balance ~ . - Limit, data = Credit)

vif(add_balance_mod)

limit_mod <- lm(Limit ~ . - Balance, data = Credit)
balance_mod_no_limit <- lm(Balance ~ . - Limit, data = Credit)

cor(resid(limit_mod), 
    resid(balance_mod_no_limit))

plot(resid(balance_mod_no_limit) ~ resid(limit_mod), 
     col = "dodgerblue", pch = 20,
     xlab = "Residuals, Added Predictor", 
     ylab = "Residuals, Original Model")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
abline(lm(resid(balance_mod_no_limit) ~ resid(limit_mod)),
       col = "darkorange", lwd = 2)

check(add_balance_mod)

diags <- diagnostics(add_balance_mod, 
                     detailed = TRUE, plotit = TRUE)

all_balance_mod <- summary(regsubsets(Balance ~ log(Income) + Limit
                                      + Rating + Cards + Age 
                                      + Education + Gender + Student 
                                      + Married + Ethnicity
                                        , data = Credit))

all_balance_mod$which

(best_r2_ind <- which.max(all_balance_mod$adjr2))

all_balance_mod$which[best_r2_ind, ]

--
p <- length(add_balance_mod$coef)
n <- nrow(Credit)

(all_balance_mod_aic <- n * log(all_balance_mod$rss / n) + 2 * (2:9))

(best_aic_ind <- which.min(all_balance_mod_aic))

all_balance_mod$which[best_aic_ind, ]

base_mod <- lm(Balance ~ log(Income) 
               + Rating + Age 
               + Student 
               + Married, data = Credit)

diags <- diagnostics(base_mod, 
                     detailed = TRUE, plotit = TRUE)

check(base_mod)

#--------------

#close
#seven_add_pred <- lm(Balance ~ (log(Income) + log(Limit) + Rating
#                     + Cards + Age ) ^ 2, data = Credit)

seven_add_pred <- lm(Balance ~ (log(Income) + log(Limit)
                     + Age) ^ 2, data = Credit)



vif(seven_add_pred)

#mod_a <- seven_add_pred


(mod_a <- step(base_mod, direction = "both", trace = 0))

diags <- diagnostics(mod_a, 
                     detailed = TRUE, plotit = TRUE)

influe_idx <- as.numeric(names(diags$cookd$influential))

base_mod_fix <- lm(Balance ~ log(Income) 
               + Rating + Age 
               + Student 
               + Married, data = Credit,
               subset = !(1:nrow(Credit) %in% influe_idx))

diags <- diagnostics(base_mod_fix, 
                     detailed = TRUE, plotit = TRUE)

check(base_mod_fix)
 
par(mfrow = c(1, 1), bg="ghostwhite")
plot(Credit$Income, Credit$Balance,  
     col = "grey", pch = 20,
     xlab = "X", ylab = "Y")

par(mfrow = c(1, 1), bg="ghostwhite")
plot(Credit$Income, Credit$Balance,  
     col = "grey", pch = 20,
     xlab = "Income", ylab = "Balance")

plot(Credit$Limit, Credit$Balance,  
     col = "grey", pch = 20,
     xlab = "Limit", ylab = "Balance")

plot(Credit$Cards, Credit$Balance,  
     col = "grey", pch = 20,
     xlab = "Cards", ylab = "Balance")

plot(Credit$Age, Credit$Balance,  
     col = "grey", pch = 20,
     xlab = "Age", ylab = "Balance")
