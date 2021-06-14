---
title: "practice-quiz-week4"
author: "AP"
date: "6/10/2021"
output: html_document
---

# Question 1

Consider a random variable X that has an F distribution with `3` and `5` degrees of freedom. Calculate $P[X > 2.7]$

```{r}
1 - pf(2.7, df1 = 3, df2 = 5)
pf(2.7, df1 = 3, df2 = 5, lower.tail = FALSE)
```

# Question 2

Question 2
For Questions 2-5, use the built-in longley dataset in R. Fit a multiple linear regression model with Employed as the response. Use three predictors: GNP, Population, and Armed.Forces. Specifically,

$Y=β_0 + β_1 x_1 + β_2x_2 + β_3 x_3 + \epsilon$

- $x_1$ is GNP 
- $x_2$ is Population 
- $x_3$ is Armed.Forces

```{r}
lg_model <- lm(Employed ~ GNP + Population + Armed.Forces, data = longley)

confint(lg_model, level = 0.9)["GNP", 1]
```

# Question 3

What is the standard error of $\hat{\beta}_2$?

```{r}
summary(lg_model)$coef["Population", "Std. Error"]
```

# Question 4

What is the p-value for testing $H_0: \beta_3 = 0 \ \text{vs} \ H_1: \beta_3 \neq 0$ 


```{r}
full_model <- lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
null_model <- lm(Employed ~ GNP + Population, data = longley)

anova(null_model, full_model)[2, "Pr(>F)"]
summary(full_model)$coef["Armed.Forces", "Pr(>|t|)"]
```

If I wanted to calculate it given the `t vale`

```{r}
t_val <- summary(full_model)$coef["Armed.Forces", "t value"]
2 * (1-pt(abs(t_val), df = nrow(longley) - 4))
2 * pt(-abs(t_val), df = nrow(longley) - 4)
```

We can get the same value using the `F statistic`

```{r}
f_stat <- anova(null_model, full_model)[2, "F"]

# 1 here is for p - q, df2 is n - p
1 - pf(f_stat, df1 = 1, df2 = nrow(longley) - 4)
```

The t statistic ^ 2 and the F statistic for the "removed" predictor `Armed.Forces` is the same.

The same can be said for the t value

# Question 5

What is the value of the $F$ test statistic for testing for significance of regression?

Wait, what the heck is f statistic again???

```{r}
summary(full_model)$fstatistic[1]
```


High values of the F statistic from the summary always tells that at least 1 predictor has a relevant linear relationship.

When we compare a full model with a null model with none of the predictors as below, the F in the anova table is the same reported in the summary for the full model.

```{r}
full_model <- lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
null_model <- lm(Employed ~ 1, data = longley)

anova(null_model, full_model)
summary(full_model)

all.equal(unname(summary(full_model)$fstatistic[1]), anova(null_model, full_model)[2, "F"])
```

```{r}
f_val <- anova(null_model, full_model)[2, "F"]
1 - pf(f_val, df1 = 4 - 1, df2 = nrow(longley) - 4)
anova(null_model, full_model)[2, "Pr(>F)"]
```



