

library(tidyverse)
library(coefplot)

# Poisson regression ------------------------------------------------------

# like the poisson distribution; used for count data 
# called using glm in similar way to logistic regression  

# formula: 
# 
# yi ~ pois(0i)

# where yi is the ith response and 
# 0i (theta i) = e^XiB 
# --> is the mean of the distribution for the ith observation



# EDA  --------------------------------------------------------------------

# subset of data from american community survey for new york state 
acs <- read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",",
                  header = TRUE, stringsAsFactors = FALSE)

acs %>% 
    ggplot(aes(x = NumChildren)) + 
    geom_histogram(binwidth = 1)

# not a perfect poisson distribution but close enough to fit a good model 



# Fit Poisson regression model --------------------------------------------

children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, 
                 data = acs, family = poisson(link = "log"))
summary(children1)
"""
Call:
glm(formula = NumChildren ~ FamilyIncome + FamilyType + OwnRent, 
family = poisson(link = 'log'), data = acs)

Deviance Residuals: 
Min       1Q   Median       3Q      Max  
-1.9950  -1.3235  -1.2045   0.9464   6.3781  

Coefficients:
                    Estimate    Std. Error z value Pr(>|z|)    
(Intercept)         -3.257e-01  2.103e-02 -15.491  < 2e-16 ***
FamilyIncome         5.420e-07  6.572e-08   8.247  < 2e-16 ***
FamilyTypeMale Head -6.298e-02  3.847e-02  -1.637    0.102    
FamilyTypeMarried    1.440e-01  2.147e-02   6.707 1.98e-11 ***
OwnRentOutright     -1.974e+00  2.292e-01  -8.611  < 2e-16 ***
OwnRentRented        4.086e-01  2.067e-02  19.773  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

Null deviance: 35240  on 22744  degrees of freedom
Residual deviance: 34643  on 22739  degrees of freedom
AIC: 61370

Number of Fisher Scoring iterations: 5
"""

# similar output to logistic regression 
# -- coefficient estimates, standard errors, p-values for coefficients 
# -- model measures of correctness = deviance and AIC 
# - general rule of thumb is that adding a variable (or a level of a factor) to 
# a model should result in a drop in deviance of two - otherwise variable not useful

# interactions and all other formula concepts work the same 

coefplot(children1)


# Overdispersion Poisson distribution  -------------------------------------

# A particular concern for Poisson distribution 
# -- the variability seen in the data is greater than theorised by the Poisson distribution 
# where the mean and variance are the same (lambda)

# overdispersion defined as 
#  OD = 1 / n-p * sum(zi^2)     i = 1, ..., n

# where  zi = (yi - yhati) / sd(yhati) = (yi - mui*theta_hat_i) / sqrt(mui*theta_hat_i)
# are the standardised residuals 

# calculating overdispersion in R 

# the standardised residuals 
z <- (acs$NumChildren - children1$fitted.values) / sqrt(children1$fitted.values)

# overdispersion factor 
sum(z^2) / children1$df.residual

# overdispersion p-value 
pchisq(sum(z^2), children1$df.residual)
## generally an overdispersion ratio of 2 or greater indicates overdispersion 
## while in this example the overdispersion ratio is less than 2, 
## the p-value = 1, indicating a statistically significant overdispersion 

## action if overdispersion? --> refit model to account for overdispersion 


# Refit overdipersed model ------------------------------------------------

# use quasipoisson family - uses the negative binomial distribution 

children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, 
                 data = acs, family = quasipoisson(link = "log"))
#same size of model as poisson - 14.9MB

multiplot(children1, children2)

# book conclusion: since overdispersion wasn't large, the second model adds just a little uncertainty 
# to the coefficient estimates... (I can't see a huge difference visually)

all.equal(children1$coefficients, children2$coefficients)
# coefficients are exactly the same - all.equal = TRUE

