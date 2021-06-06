# Question 1

pt(1.3, 7)
pt(1.3, 7, lower.tail = FALSE)
1 - pt(1.3, 7)

# Question 2

qt(0.025, 9, lower.tail = FALSE)
qt(1 - 0.025, 9)

pt(2.364624, 7)

# Question 3
?trees
trees_model <- lm(Girth ~ Height, data = trees)
summary(trees_model)$coefficients["Height", "Pr(>|t|)"]

# Question 4
conf_interval <- confint(trees_model, parm = "Height", level = 0.90)
conf_interval[1, 2] - conf_interval[1, 1]

# Question 5
predict(trees_model, newdata = data.frame(Height = c(79)),
        interval = c("confidence"), level = 0.95)
