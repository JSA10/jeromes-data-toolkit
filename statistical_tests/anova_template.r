
# the natural next step after comparing two groups using a t.test is to compare 
# many groups

# anova is a widely taught but relatively old fashioned technique, an alternative is 
# fitting a linear regresssion model with one category variable and no intercept 

# Anova formula generates an F - Statistic 

data(tips, package = "reshape2")
head(tips)
summary(tips)


tip_anova <- aov(tip ~ day - 1, tips)

tip_intercept <- aov(tip ~ day, tips)

tip_anova$coefficients

tip_intercept$coefficients

# removing intercept makes analysis more straightforward 
# 
# The ANOVA tests whether any groupn is different from any other group, but does 
# not specify which group is different. 

# summary just prints a single p-value 
summary(tip_anova)

# simplest way to see which group differed --> visualise 

library(dplyr)

#summarise mean and approximate CI based on 2 * sd from mean 
tip_day_summary <- tips %>% 
    group_by(day) %>% 
    summarise(
        tip.mean = mean(tip),
        tip.sd = sd(tip),
        lower = tip.mean - 2 * tip.sd / sqrt(nrow(tips)),
        upper = tip.mean + 2 * tip.sd / sqrt(nrow(tips))
    )

#plot mean and error bars 
tip_day_summary %>% 
    ggplot2::ggplot(aes(x = tip.mean, y = day)) + 
    geom_point() +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3)
# plot shows error bars overlap 

## note NROW works more consistentlty than nrow which only works with data frames 
## and matrices 



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




