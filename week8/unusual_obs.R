par(mfrow = c(1, 3))
set.seed(42)
ex_data  = data.frame(x = 1:10,
                      y = 10:1 + rnorm(n = 10))
ex_model = lm(y ~ x, data = ex_data)

# low leverage, large residual, small influence
point_1 = c(5.4, 11)
ex_data_1 = rbind(ex_data, point_1)
model_1 = lm(y ~ x, data = ex_data_1)
plot(y ~ x, data = ex_data_1, cex = 2, pch = 20, col = "grey",
     main = "Low Leverage, Large Residual, Small Influence")
points(x = point_1[1], y = point_1[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_1, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, small residual, small influence
point_2 = c(18, -5.7)
ex_data_2 = rbind(ex_data, point_2)
model_2 = lm(y ~ x, data = ex_data_2)
plot(y ~ x, data = ex_data_2, cex = 2, pch = 20, col = "grey",
     main = "High Leverage, Small Residual, Small Influence")
points(x = point_2[1], y = point_2[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_2, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, large residual, large influence
point_3 = c(14, 5.1)
ex_data_3 = rbind(ex_data, point_3)
model_3 = lm(y ~ x, data = ex_data_3)
plot(y ~ x, data = ex_data_3, cex = 2, pch = 20, col = "grey", ylim = c(-3, 12),
     main = "High Leverage, Large Residual, Large Influence")
points(x = point_3[1], y = point_3[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_3, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# leverages
hval_1 <- hatvalues(model_1)
hval_2 <- hatvalues(model_2)
hval_3 <- hatvalues(model_3)

# large leverages
hval_1 > 2 * mean(hval_1)
hval_2 > 2 * mean(hval_2)
hval_3 > 2 * mean(hval_3)

# standardized residuals
rst_1 <- rstandard(model_1)
rst_2 <- rstandard(model_2)
rst_3 <- rstandard(model_3)

# standardized residuals are close to be standard normal
# so values that are 2 times away from the the standard deviation
# which is 1, should be pretty rare.
rst_1[abs(rst_1) > 2]
rst_2[abs(rst_2) > 2]
rst_3[abs(rst_3) > 2]

# Cook's distance
cookd_1 <- cooks.distance(model_1)
cookd_2 <- cooks.distance(model_2)
cookd_3 <- cooks.distance(model_3)

# A know heuristic to consider a large Cook's distance
# is 4 divided by the number of points

# here turns out that point 1 seems to have a big influence,
# but it's really point 11 the one that is pulling the 
# regression line.
cookd_3 > 4 / length(cookd_3)

# mtcars example

mpg_hp_add <- lm(mpg ~ hp + am, data = mtcars)
mpg_hatvals <- hatvalues(mpg_hp_add)
mpg_rstan <- abs(rstandard(mpg_hp_add))
mpg_hp_add_cd <- cooks.distance(mpg_hp_add)

sum(mpg_hatvals > 2 * mean(mpg_hatvals))
sum(mpg_rstan > 2)
sum(mpg_hp_add_cd > 4 / length(mpg_hp_add_cd))

large_cd_mpg <- mpg_hp_add_cd > 4 / length(mpg_hp_add_cd)
mpg_hp_add_cd[large_cd_mpg]

coef(mpg_hp_add)
mpg_hp_add_fix <- lm(mpg ~ hp + am, data = mtcars, 
    subset = mpg_hp_add_cd <= 4 / length(mpg_hp_add_cd))

# the difference in the coefficients is not that big
# after removing those 2 influential points
coef(mpg_hp_add_fix)

par(mfrow = c(2, 2))
plot(mpg_hp_add)


# autompg example

autompg <- read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE
)
#give headers
colnames(autompg) <- c("mpg","cyl","disp","hp","wt","acc","year","origin","name")
# remove missing data marked as ?
autompg <- subset(autompg, autompg$hp != "?")
# remove a record that has issues
autompg <- subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) <- paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the name and origin columns
autompg <- subset(autompg, select = c("mpg","cyl","disp","hp","wt","acc","year", "origin"))
# change horsepower to numeric
autompg$hp <- as.numeric(autompg$hp)

# create a dummy variable for origin
autompg$domestic <- as.numeric(autompg$origin == 1)

# Remove 3 and 4 cyl cars
autompg <- autompg[autompg$cyl != 3, ]
autompg <- autompg[autompg$cyl != 5, ]

unique(autompg$cyl)
autompg$cyl <- as.factor(autompg$cyl)

big_model <- lm(mpg ~ disp * hp * domestic, data = autompg)

par(mfrow = c(1, 2))
qqnorm(resid(big_model), col = "darkgrey")
qqline(resid(big_model), col = "dodgerblue", lwd = 2)

big_model_cd <- cooks.distance(big_model)
sum(big_model_cd > 4 / length(big_model_cd))

shapiro.test(resid(big_model))

## Check how we do if we remove the "influencers"
big_model_fix <- lm(mpg ~ disp * hp * domestic, data = autompg,
                    subset = big_model_cd <= 4 / length(big_model_cd))

qqnorm(resid(big_model_fix), col = "darkgrey")
qqline(resid(big_model_fix), col = "dodgerblue", lwd = 2)

# High p-value, so we are not concerned about normality
# violations.
shapiro.test(resid(big_model_fix))

# We haven't justified the removal of those data points just because the QQ plot is
# better. Before that, we should try to change our model or some transformations.

