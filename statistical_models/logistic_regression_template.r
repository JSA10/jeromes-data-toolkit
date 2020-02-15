
library(tidyverse)

#not all data can be modelled with linear regression, for example: 
#
# -- binomial (TRUE/FALSE)
# -- categories (nominal / ordinal)
# -- counts 
# -- events in a time frame 

## Generalised linear models developed for these data 
# they arte still modeled using a linear predictor XB, but they are transformed 
# using some link function 


# Logistic regression  ----------------------------------------------------

# powerful and commonly used (particularly in marketing and medicine)


# logistic regression models formulated as 
# -- p(yi = 1) = logit^-1(XiB)

# where yi is the ith outcome / response and XiB is the linear predictor 

# the inverse logit function transforms the continuous input from the linear predictor 
# to fall between 0 and 1 
# - logit ^ -1 (x) = e^x / 1 + e^x = 1 / 1 + e^-x 
# this is the inverse of the link function 


# EDA  --------------------------------------------------------------------

# subset of data from american community survey for new york state 
acs <- read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",",
                  header = TRUE, stringsAsFactors = FALSE)


# need to create a binary variable that determines whether a household has an 
# income greater than $150,000 

acs$Income <- with(acs, FamilyIncome >= 150000)

summary(acs)


# first glm for Income  ---------------------------------------------------

income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType, 
               data = acs, family = binomial(link = "logit"))
summary(income1)

"""
Call:
glm(formula = Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + 
FamilyType, family = binomial(link = 'logit'), data = acs)

Deviance Residuals: 
Min       1Q   Median       3Q      Max  
-2.8452  -0.6246  -0.4231  -0.1743   2.9503  

Coefficients:
Estimate Std. Error z value Pr(>|z|)    
(Intercept)         -5.738e+00  1.185e-01 -48.421   <2e-16 ***
HouseCosts           7.398e-04  1.724e-05  42.908   <2e-16 ***
NumWorkers           5.611e-01  2.588e-02  21.684   <2e-16 ***
OwnRentOutright      1.772e+00  2.075e-01   8.541   <2e-16 ***
OwnRentRented       -8.886e-01  1.002e-01  -8.872   <2e-16 ***
NumBedrooms          2.339e-01  1.683e-02  13.895   <2e-16 ***
FamilyTypeMale Head  3.336e-01  1.472e-01   2.266   0.0235 *  
FamilyTypeMarried    1.405e+00  8.704e-02  16.143   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 22808  on 22744  degrees of freedom
Residual deviance: 18073  on 22737  degrees of freedom
AIC: 18089

Number of Fisher Scoring iterations: 6
"""

# similar summary output to lm 
# -- coefficient estimates, standard errors, p-values for coefficients 
# -- model measures of correctness = deviance and AIC 
# - general rule of thumb is that adding a variable (or a level of a factor) to 
# a model should result in a drop in deviance of two - otherwise variable not useful

# interactions and all other formula concepts work the same 

coefplot::coefplot(income1)




# interpreting coefficients from logistic regression ----------------------

# need to take inverse logit 

inv_logit <- function(x) {
    1 / (1 + exp(-x))
}

inv_logit(income1$coefficients)

