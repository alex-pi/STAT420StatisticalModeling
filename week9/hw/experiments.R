library(leaps)

add_balance_mod <- lm(Balance ~ ., data = Credit)

all_balance_mod <- summary(regsubsets(Balance ~ ., data = Credit))

all_balance_mod$which

(best_r2_ind <- which.max(all_balance_mod$adjr2))

all_balance_mod$which[best_r2_ind, ]

p = length(add_balance_mod$coef)
n = nrow(Credit)

(all_balance_mod_aic = n * log(all_balance_mod$rss / n) + 2 * (2:9))

(best_aic_ind = which.min(all_balance_mod_aic))

#--------------

seven_add_pred <- lm(Balance ~ Income + Limit 
                     + Cards + Age + Gender + Student, data = Credit)

vif(seven_add_pred)

mod_a <- seven_add_pred


(mod_a <- step(seven_add_pred, direction =  "backward", trace = 0))

diags <- diagnostics(seven_add_pred, 
                     detailed = TRUE, plotit = TRUE)

get_loocv_rmse(mod_a)
get_adj_r2(mod_a)
get_bp_decision(mod_a, alpha = 0.01)
get_num_params(mod_a)
 
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