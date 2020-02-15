#correlation and covariance 

# When dealing with more than one variable, we need to test their relationships 
# with each other 

# Two simple methods are correlation and covariance 

# example usinfg economics data from ggplot2 

require(ggplot2)
head(economics)
str(economics)
summary(economics)
?economics

# US economic time series, monthly observations starting July 1967, ending April 2015 
# date
# pce = personal consumption expenditures in $ (billions)
# pop = total population (000s)
# psavert = personal savings rate (range = 1 - 17) 
# uempmed = median duration of unemployment (weeks)
# unemploy = number of unemployed (000s)


# Correlation -------------------------------------------------------------


# corrlation between two numeric variables = calculation for how closely two or 
# more variables move together. Measure = r and ranges from -1 to 1
cor(economics$pce, economics$psavert)
# strong, negative correlation = consumption and spending are opposites 

#correlation defined as sum of square difference of each data point to the mean 
# divided by n - 1 * product of standard deviations
sum((x - mean(x))(y - mean(y)) / ((nrow(x) - 1) * sd(x) * sd(y)))

## In J Lander book they break it down outside of a function 
sum_square_x <- economics$pce - mean(economics$pce)
sum_square_Y <- economics$psavert - mean(economics$psavert)
n_minus_1_xy <- nrow(economics) - 1
sd_x <- sd(economics$pce)
sd_y <- sd(economics$psavert)
#correlation formula 
sum(sum_square_x * sum_square_Y) / (n_minus_1_xy * sd_x * sd_y)


# to compare multiple variables at once, use cor on a matrix (or numeric df)
cor(economics[, c(2, 4:6)])
# rather than list of numbers, helpful to visualise 
# can use GGally::ggpairs or corplot

library(corrplot)
cor_econ <- cor(economics[, c(2, 4:6)])
corrplot(cor_econ)
corrplot(cor_econ, method = "number")
corrplot(cor_econ, method = "color")
corrplot(cor_econ, type = "upper", method = "number", diag = FALSE)

# other arguments avaiable in cor function 
# na.rm 
# use = character string indicating more options for handling missing values 
# = "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
# method = type of correlation calculation 
# = "pearson", "kendall", "spearman"



# Covariance  -------------------------------------------------------------

# similar to correlation but calculates the variance between variables 

# covariance defined as the inverse of the length of the data - 1 multiplied by 
# the sum of squares 

# cov(X, Y) function works similary to cor and has same arguments 

cov(economics$pce, economics$psavert)
# - 9361.028
cov(economics[, c(2, 4:6)])
# how intuit the results? 
"""
You can use the covariance to determine the direction of a linear relationship 
between two variables as follows:

- If both variables tend to increase or decrease together, the coefficient is positive.
- If one variable tends to increase as the other decreases, the coefficient is negative.

Data is not standardised, so can't use to assess the strength of a linear relationship 
- that = correlation 
Above from: 
https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/regression/how-to/covariance/interpret-the-results/

Below from: 
https://www.investopedia.com/ask/answers/041515/how-do-you-interpret-magnitude-
covariance-between-two-variables.asp

The magnitude of covariance may be skewed whenever the data set contains too 
many significantly different values.

A single outlier in the data can dramatically change the calculation and 
overstate or understate the relationship. 
"""


# check cov and cor * sdx * sdy = identical 
identical(cov(economics$pce, economics$psavert),
          cor(economics$pce, economics$psavert) * sd_x * sd_y
          )
# [1] TRUE



# Appendix - functions  ---------------------------------------------------

sum((x[i] - mean(x))(y[i] - mean(y)) / ((length(x) - 1) * sd(x)*sd(y)))

# attempt to code up a function from scratch 
rxy = function(x, y) {
    for(i in 1:seq_len(x)){
        sum_squares = (x[i] - mean(x))(y[i] - mean(y))
    }
    sums_squares / ((length(x) - 1) * sd(x)*sd(y))
}

# unable to locate formular for cor function so moving on 
rxy(economics$pce, economics$psavert)
methods(cor)
getAnywhere(C_cor)
