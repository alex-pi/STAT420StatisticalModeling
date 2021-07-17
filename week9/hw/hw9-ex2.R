#add_balance_mod <- lm(Balance ~ . - Income + log(Income), data = Credit)

(add_balance_mod <- lm(Balance ~ . - Income + log(Income)
                       , data = Credit))

(mod_a <- step(add_balance_mod, direction = "both"
               , trace = 0, k = log(nrow(Credit))))

check(lm(Balance ~ Limit
         + poly(Cards, degree = 3) 
         + poly(Age, degree = 2)
         + Student + log(Income)
         , data = Credit))

diags <- diagnostics(lm(Balance ~ Limit
                        + poly(Cards, degree = 3) 
                        + poly(Age, degree = 2)
                        + Student + log(Income)
                        , data = Credit), 
                     detailed = TRUE, plotit = TRUE)

mod_1b <- lm(Balance ~ (Limit +
            + Cards 
            + Age
            + Student 
            + Married
            + log(Income)) ^ 2
            , data = Credit)

(mod_b1 <- step(mod_1b, 
                direction = "backward"
               , trace = 0))



check(mod_b1)

diags$bptest

diags$p_val

diags$decision

names(Credit)
