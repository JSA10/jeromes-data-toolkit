
# implementing a bayesian t-test using BEST 

library(tidyverse)
library(BEST)
library(coefplot)


# EDA ---------------------------------------------------------------------


# compare initially to t.test using same data from jared lander book example 

data(tips, package = "reshape2")
glimpse(tips)
summary(tips)

# dataset for tips received by one waiter over a period of a few months 
# variables collected: 
# total_bill (3.07 - 50.81)
# tip (range = 1 - 10)
# sex of bill payer (Female / Male)
# smoker (No / Yes)
# day (Thurs / Fri / Sat / Sun)
# time (Dinner / Lunch) 
# size of the party (1 - 6)


# Intro to Bayesian Estimation and BEST package -------------------------------

#BEST = Bayesian Estimation Superceded the T-test 
# An alternative to t-tests, providing posterior estimates for groups means and 
# standard deviations, with their differences and effect sizes. 

# Bayesian estimation provides a much richer view of the data and can be summarised 
# as point estimates and credible intervals to mirror summary of t.tests 

# Bayesian approach results in probability statements about the values of interest, 
# rather than p-values and significance levels

# In addition, the procedure accounts for departures from normality by using a 
# t-distribution to model the variable of interest and estimating a measure of 
# normality.


# Fitting a one-sided bayesian t-test using BEST --------------------------



#fitting a two sided t - test on one variable 
tips1 <- BEST::BESTmcmc(tips$tip, y2 = 2.5)
# sets the test up so mu = the null hypothesis and we are interested in whether
# the data falls within or on either side of the null confidence interval 

plot(tips1)


# comparing two samples ---------------------------------------------------

tips_male <- tips[tips$sex == "Male", ]
tips_female <- tips[tips$sex == "Female",]

tips2 <- BEST::BESTmcmc(tips_male$tip, tips_female$tip)

plot(tips2)
# when comparing differences in means, a difference of 0 is in the credible interval 
# so while more of the male tips are likely to be higher 81.5% of posterior for 
# difference > 0, the difference is not significantly different (review correct terminology for BEST)


# One sample, no comparison -----------------------------------------------

tips3 <- BEST::BESTmcmc(tips$tip)

plot(tips3)
BEST::plotAll(tips3)
