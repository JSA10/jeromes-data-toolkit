
library(UsingR)
library(tidyverse)

# Definition - simple linear regression ----------------------------------

# The workhorse of statistical analysis 
# - invented by Francis Galton to study relationships btw parents and children,
# which he described as regressing to the mean.  
#  
# - at it's simplest its used to determine relationship between two variables 
# - given one variable, it tells us what we can expect from the other 

# Terminology: 
# *Outcome*, response, predicted variable is the Y variable that we want to understand 
# or predict  

# *Predictor*, explanatory, input variables are what we use to understand what drives 
# the outcome variable and predict an average value for it

## The relationship defined as; 
# y = a + bx + e 

# where b is the coefficient for predictor x and is calculated as the sum of square 
# b = sum((x - mean(x))(y = mean(y))) / sum((x - mean(x)) ^ 2)
# a is the y-intercept 
# a = mean(y) - b  
# e = errors or residuals, which are normally distributed 
# e = N(O,1)


# Fitting simple regression model -----------------------------------------


summary(father.son)

# fitting a simple lm model visually using geom_smooth 
father.son %>% 
    ggplot(aes(x = fheight, y = sheight)) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(x = "Fathers", y = "Sons")

# to make the results available to us
heights_lm <- lm(sheight ~ fheight, data = father.son)
heights_lm 
# shows simplified formula with intercept and coefficient
# intercept (a) = 33.8866, fheight coefficient (b) = 0.5141

# for more details and evaluation statistics for model 
summary(heights_lm)
# prints standard errors, t-test values, p-values for the coefficients, 
# degrees of freedom, residual summary stats and the results of an F-test for the 
# model. 

# to calculate RMSE: 
sqrt(mean(heights_lm$residuals ^ 2))
## what's the difference btw RMSE and residual standard error from model summary 
## Residual standard error = 2.437 in this example 


# Using linear regression as an alternative to ANOVA ----------------------

# fit a regression model with just one categorical variable and no intercept term 

data(tips, package = "reshape2")
summary(tips)

tips_anova <- aov(tip ~ day - 1, data = tips)
# putting -1 in the formula indicates that the intercept should not be included
# categorical variable day automatically setup to have a coefficient for each level 

tips_lm <- lm(tip ~ day - 1, data = tips)

summary(tips_anova)
"""
Df Sum Sq Mean Sq F value Pr(>F)    
day         4 2203.0   550.8   290.1 <2e-16 ***
Residuals 240  455.7     1.9                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
"""

summary(tips_lm)
"""
Call:
lm(formula = tip ~ day - 1, data = tips)

Residuals:
Min      1Q  Median      3Q     Max 
-2.2451 -0.9931 -0.2347  0.5382  7.0069 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
dayFri    2.7347     0.3161   8.651 7.46e-16 ***
daySat    2.9931     0.1477  20.261  < 2e-16 ***
daySun    3.2551     0.1581  20.594  < 2e-16 ***
dayThur   2.7715     0.1750  15.837  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.378 on 240 degrees of freedom
Multiple R-squared:  0.8286,	Adjusted R-squared:  0.8257 
F-statistic: 290.1 on 4 and 240 DF,  p-value: < 2.2e-16
"""

# same F-Statistic and degrees of freedom as Anova
# -- > Anova and regressions both devised along the same lines (linear models)

# Can summarise coeficients and standard errors visually manually (see example in anova_template.r)

#summarise mean and approximate CI based on 2 * sd from mean 
tip_day_summary <- tips %>% 
    group_by(day) %>% 
    summarise(
        tip.mean = mean(tip),
        tip.sd = sd(tip),
        N = NROW(tip),
        tfrac = qt(p = 0.90, df = N - 1), 
        lower = tip.mean - tfrac * tip.sd / sqrt(N),
        upper = tip.mean + 2 * tip.sd / sqrt(N)
    )
# note vs. Anova example, 2 sds from mean is replaced by t value using quantile function 


# Or by extracting coefs from model summary  
tips_lm_info <- summary(tips_lm)
tips_lm_coefs <- as.data.frame(tips_lm_info$coefficients[ , 1:2])
tips_lm_coefs <- tips_lm_coefs %>% 
    dplyr::mutate(
        lower = Estimate - qt(p = 0.90, df = tips_lm_info$df[2]) * `Std. Error`,
        upper = Estimate + qt(p = 0.90, df = tips_lm_info$df[2]) * `Std. Error`,
        day = rownames(tips_lm_coefs)
    )

tips_lm_coefs %>% 
    ggplot2::ggplot(aes(x = Estimate, y = day)) +
    geom_point() +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3)

