
library(tidyverse)
library(DataExplorer)
library(coefplot)



# Multiple regression -----------------------------------------------------

# logical extension of simple regression, allows for multiple predictors 

# math requires matrix algebra, but lm function applied with little extra effort 

# relationship between response and p predictors (intercept + p - 1 predictors) 
# is modelled as 

# Y = XB + e

# Y = n * 1 response vector 
# X = n * p predictors matrix (n rows, intercept + p - 1 predictors)
# B = p * 1 coefficients vector (one for each predictor and the intercept)
# e = n * 1 errors vector (normally distributed: e ~ N(0,1) )

# Solution for coefficients is simply written as 
# B_hat = (Xt * X)^-1 * Xt * Y


# Example using NYC housing data ------------------------------------------

# curated dataset originally from https://www.data.cityofnewyork.us/ 

ny_housing <- read.table("http://www.jaredlander.com/data/housing.csv", sep = ",",
                         header = TRUE, stringsAsFactors = FALSE)
write.csv(ny_housing, "ny_housing_jared_lander.csv")

summary(ny_housing)
glimpse(ny_housing)

# outcome (response) variable is value per square foot, everything else is potential 
# predictor 
# - can ignore income and expense variables 

#ny_housing <- ny_housing %>% 
#    dplyr::select(-c(Net.Operating.Income, Estimated.Expense))


# Example: step 1 visualise - EDA  ----------------------------------------


DataExplorer::plot_bar(ny_housing)
# ignored 151 neighbourhood categories 
# bar charts for builkding classification and boro 
#   (dataset mainly covers Manhattan, Brooklyn and Queens)
#   Most classed as R4-CONDOMINIUM
DataExplorer::plot_histogram(ny_housing)
# bimodal distribution of value and income per sq foot 
# - hypothesise boro has a big impact 

# highly right skewed distributions for total sq feet and number of units 
# see bit more of distribution when remove buildings with more than 1k units 
DataExplorer::plot_histogram(ny_housing[ny_housing$Total.Units < 1000, ]) 

# just 6 buildings need to be removed 
sum(ny_housing$Total.Units >= 1000)
ny_housing <- ny_housing[ny_housing$Total.Units < 1000, ]

# exploring boro 

ny_housing %>% 
    ggplot2::ggplot(aes(x = Market.Value.per.SqFt, fill = Boro)) +
    geom_histogram(binwidth = 10)
# first peak is mainly brookly and queens with manhattan generally higher values 

# see distributions better with facets
ny_housing %>% 
    ggplot2::ggplot(aes(x = Market.Value.per.SqFt, fill = Boro)) +
    geom_histogram(binwidth = 10) +
    facet_wrap(~ Boro)
#queens has bimodal dist, brooklyn tight and low dist, manhattan wide range but normal


pairs(dplyr::select_if(ny_housing, is.numeric))
#DataExplorer::plot_scatterplot(dplyr::select_if(ny_housing, is.numeric), by = ?) 

# even after removing outliers, seems like a log transformation of some data helpful 
# the book log transforms SqFt, ValuePerSqFt and Units 



# Example - fitting lm model to NYC data ----------------------------------
names(ny_housing)
house1 <- lm(Market.Value.per.SqFt ~ Total.Units + Gross.SqFt + Boro, data = ny_housing)
#Boro will be converted to a factor 

summary(house1)
# prints out: 
# - how the function was called, 
# - quantiles for residuals, 
# - coefficient estimates, standard errors and p-values for each variable 
# - degrees of freedom, p-value and F-statistics for the model 

"""
Call:
lm(formula = Market.Value.per.SqFt ~ Total.Units + Gross.SqFt + 
Boro, data = ny_housing)

Residuals:
Min       1Q   Median       3Q      Max 
-168.458  -22.680    1.493   26.290  261.761 

Coefficients:
                   Estimate   Std. Error  t value Pr(>|t|)    
(Intercept)        4.430e+01  5.342e+00   8.293  < 2e-16 ***
Total.Units       -1.532e-01  2.421e-02  -6.330 2.88e-10 ***
Gross.SqFt         2.070e-04  2.129e-05   9.723  < 2e-16 ***
BoroBrooklyn       3.258e+01  5.561e+00   5.858 5.28e-09 ***
BoroManhattan      1.274e+02  5.459e+00  23.343  < 2e-16 ***
BoroQueens         3.011e+01  5.711e+00   5.272 1.46e-07 ***
BoroStaten Island -7.114e+00  1.001e+01  -0.711    0.477    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 43.2 on 2613 degrees of freedom
Multiple R-squared:  0.6034,	Adjusted R-squared:  0.6025 
F-statistic: 662.6 on 6 and 2613 DF,  p-value: < 2.2e-16
"""
# no coefficent for the Bronx because that is the baseline level of Boro and 
# all the other Boro coeffs are relative to that value (discussed in replacemnent for ANOVA?)

# NOTES for interpretation 
# -- the coefficients represent the effect of the predictors on the response 
# and the standard errors are the uncertainty in the estimation of the coefficients. 

# -- t-value (statistic) and p-value for coefficients are numerical measures of statistical 
# significance, though should be viewed with caution as modern data scientists (?) 
# do not like to look at significance of individual coefficients but rather judge 
# the model as a whole (see chapter 18 in jared lander book) 

# -- model p-value and F-stat are measures of its goodness of fit 
# -- degrees of freedom for regression calculated as number of obs minus number of 
# coefficients 

#quick ways to get coefficients 
house1$coefficients
coef(house1)
coefficients(house1)


# visualising coefficients  -----------------------------------------------

coefplot::coefplot(house1)
# figure shows, as expected that being located in Manhattan has largest effect on 
# value per sq ft 
# suprisingly, number of units or sq foot in building has little impact on value 


# adding interaction terms ------------------------------------------------

# -- this model has purely additive terms, interactions between variables can 
# be equally powerful 

# rather than separating with +, using * included individual variables and their interaction 
# using : just included the interaction, not the individual variables 


house2 <- lm(Market.Value.per.SqFt ~ Total.Units * Gross.SqFt + Boro, data = ny_housing)
house3 <- lm(Market.Value.per.SqFt ~ Total.Units:Gross.SqFt + Boro, data = ny_housing)

coefplot(house2)
coefplot(house3)
#little impact of combining two variables 


## three variables interacting together
house4 <- lm(Market.Value.per.SqFt ~ Total.Units * Gross.SqFt * Gross.Income.per.SqFt, 
             data = ny_housing)
# the resulting coefficients will be the three individual terms, three two-way interactions 
# and one three-way interaction 

house4$coefficients 
coefplot(house4)


#interacting a continuous variable with a factor / categorical variable results in: 
# - individual terms for the continuous variable and each non-baseline level of the 
# factor, ]
# - plus an interaction term between the continuous variable and each level 

# interacting two or more factors results in terms for each individual non-baseline 
# level and an interaction term for every combination of non-baseline level

house5 <- lm(Market.Value.per.SqFt ~ Building.Classification * Boro, 
             data = ny_housing)

house5$coefficients
coefplot(house5)


# ratios in models 

# because SqFt nor Units appear to be significant in any model, it would be good to 
# test their ratio. A division in a formula must be wrapped in the I function
house6 <- lm(Market.Value.per.SqFt ~ I(Gross.SqFt / Total.Units) + Boro, data = ny_housing)
coefplot(house6)

# the I function is used to preserve a mathematical relationship in a formula and 
# prevent it being interpreted according to formula rules 
# e.g. - using (Units + Sqft)^2 in a formaula is same as Units * SqFt, 
# whereas I(Units * SqFt)^2 will include the square of the sum of the two variables 
# as a term in the formula 



# visualising coefs for multiple models -----------------------------------

coefplot::multiplot(house1, house2, house3)







# Make predictions --------------------------------------------------------

ny_housing_test <- read.table("http://www.jaredlander.com/data/housingNew.csv", sep = ",",
                              header = TRUE, stringsAsFactors = FALSE)

#Making a prediction can be as simple as calling predict

# be careful with factor predictors to ensure they have same levels as those 
# used in building the model 

names(ny_housing) <- names(ny_housing_test)

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = ny_housing)

#make prediction with new data and 95% Confidence bounds 
ny_house_predict <- predict(house1, newdata = ny_housing_test, se.fit = TRUE, 
                            interval = "prediction", level = 0.95)

# view predictions with upper and lower bounds based on standard errors 
head(ny_house_predict$fit)
summary(ny_house_predict$fit)

#view the standard errors for prediction 
head(ny_house_predict$se.fit)
summary(ny_house_predict$se.fit)

summary(ny_house_predict)

# other useful lm arguments 
# - weight --> specifies weights attributed to observations (both probability and counts)
# - subset --> fit the model only on a subset of the data 







