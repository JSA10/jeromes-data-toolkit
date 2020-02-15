
# t.test - invented by william gosset while working at the Guiness brewery 
# taught for conducting tests on the mean of data or for comparing two sets of data 

#walk through with jared lander example 

install.packages("coefplot")
library(coefplot)

data(tips, package = "reshape2")
head(tips)
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


# One sample t test -------------------------------------------------------


"""
- The test calculates the mean of the data and builds a confidence interval

- if the value being tested falls within that confidence interval then we can 
conclude that it is the true value for the mean of the data, 
- otherwise; we conclude that it is not the true mean 
"""

#fitting a two sided t - test on one variable 
t.test(tips$tip, alternative = "two.sided", mu = 2.5)
# sets the test up so mu = the null hypothesis and we are interested in whether
# the data falls within or on either side of the null confidence interval 

"""
One Sample t-test

data:  tips$tip
t = 5.6253, df = 243, p-value = 5.08e-08
alternative hypothesis: true mean is not equal to 2.5
95 percent confidence interval:
2.823799 3.172758
sample estimates:
mean of x 
2.998279 
"""

# the p-value indicates the null hypothesis that the mean = $2.50 
# should be rejected, and we conclude that the mean is not equal to 2.50 

#the t-statistic is the ratio where:
# - the numerator is the difference between the estimated mean and the hypothesised mean 
# - the denominator is the standard error of the estimated mean 

t_statistic <- (mean(x) - mu_h0) / (sd(x) / sqrt(nrow(x)))

# if the hypothesised mean is correct, then we'd expect the t-statistic to fall 
# somewhere in the middle of the distribution, somewhere about two sd's from the mean 
# - of the t distribution 



# t.test args -------------------------------------------------------------

t.test(x, 
       y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = 0.95, ...)

# alternative hypothesis - can just use initial letter 
# mu = true value of the mean (or difference in means if two-sample test)
# paired = logical -  want a paired t-test? 
# var.equal = logical - are two variances equal? 
#   if TRUE - then pooled variance used to estimate variance
#   otherwise uses Welch (or Satterthwaite) approximation to the degrees of freedom 
# conf.level = confidence level of the interval 



# Visualise t - distribution and one sample, two-sided t.test ---------------

#build a t dist 
randomT <- rt(3000, df=nrow(tips) - 1)

#get t-statistic and other info 
tip_t_test <- t.test(tips$tip, alternative = "two.sided", mu = 2.50)

#visualise t distribution 
ggplot2::ggplot(data.frame(t_distribution = randomT)) +
    geom_density(aes(x = t_distribution), fill = "grey", colour = "grey") +
    geom_vline(xintercept = tip_t_test$statistic) +
    geom_vline(xintercept = mean(randomT) + c(-2, 2) * sd(randomT), linetype = 2)
 


# t-test concepts ---------------------------------------------------------

# p-value is the probability, if the null hypothesis is correct of getting the 
# observed data or more extreme a result 
# It is a measure of how extreme the statistic is (in this case - estimated mean)

# one of big problems with p-values is determining what is considered as too extreme 

# degrees of freedom = the effective number of observations 
# generally df = number of observations - number of parameters being estimated 
# in this case, just estimating standard error so: df = nrow - 1 


# One-sided t.test  -------------------------------------------------------

t.test(tips$tip, alternative = "greater", mu = 2.50)

"""
	One Sample t-test

data:  tips$tip
t = 5.6253, df = 243, p-value = 2.54e-08
alternative hypothesis: true mean is greater than 2.5
95 percent confidence interval:
2.852023      Inf
sample estimates:
mean of x 
2.998279 

"""

# p-value indicates a value of 2.50 is unlikely and that the true mean is 
# greater than 2.50 



# Two sample t.test - pt.1 check variance ---------------------------------------

# two sample t-test = comparing means 

# first need to check variance of each variable 
#   a traditional t-test requires both to have equal variance 
#   Welch two-sample t-test can handle groups with differing variances 


# 1 compute variance for each group 
aggregate(tip ~ sex, data = tips, var)
# variance not equal 

# NOTE: could do the above using group_by + summarise dplyr 

# 2 test for normality 
shapiro.test(tips$tip)
shapiro.test(tips$tip[tips$sex == 'Female'])
shapiro.test(tips$tip[tips$sex == 'Male'])
## all the tests fail - so need to inspect visually 


ggplot2::ggplot(tips, aes(x = tip, fill = sex)) + 
    geom_histogram(binwidth = 0.5, alpha = 1/2) 
# alpha = transparency of colours 


ggplot2::ggplot(tips, aes(x = tip, fill = sex)) + 
    geom_histogram(binwidth = 0.5, alpha = 1/2) +
    facet_grid(sex ~ .)

# looks normal with a left skew (tail to right)

# J.lander book states doesn't look normal, so neither 
# - standard F-Test (var.test) 
# - bartlett test (bartlett.test) 
# will suffice 

# use Ansari Bradley test
ansari.test(tip ~ sex, tips)
"""
	Ansari-Bradley test

data:  tip by sex
AB = 5582.5, p-value = 0.376
alternative hypothesis: true ratio of scales is not equal to 1
"""
# this test indicates variances are equal, meaning we can use standard two-sample t-test 


# Two-sample t.test - pt 2 - comparing means ---------------------------


# standard two-sample t-test var.equal = FALSE would run the Welch test 
t.test(tip ~ sex, data = tips, var.equal = TRUE)

"""
	Two Sample t-test

data:  tip by sex
t = -1.3879, df = 242, p-value = 0.1665
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-0.6197558  0.1074167
sample estimates:
mean in group Female   mean in group Male 
2.833448             3.089618 
"""
# indicates that the difference between tips from males and females is not significantly 
# different (results not significant) and we can conclude that male and female diners 
# tip similar amounts

# statistical rigour = good but a simple rule of thumb could be whether two means 
# are within two standard deviations of each other 
library(dplyr)

tip_sex_summary <- tips %>% 
    group_by(sex) %>% 
    summarise(
        tip.mean = mean(tip),
        tip.sd = sd(tip),
        lower = tip.mean - 2 * tip.sd / sqrt(nrow(tips)),
        upper = tip.mean + 2 * tip.sd / sqrt(nrow(tips))
    )

# always good to visualise 

tip_sex_summary %>% 
    ggplot2::ggplot(aes(x = tip.mean, y = sex)) + 
    geom_point() +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2)
# plot shows error bars overlap 


# Paired two-sample t-test ------------------------------------------------

# used for testing paired data - measurements on twins, before and after treatments 
# father and son comparisons etc. 

install.packages("UsingR")
library(UsingR)

summary(father.son)
# two variables 
# - fheight ranges from 59.01 to 75.43 
# - sheight ranges from 58.51 to 78.36

t.test(father.son$fheight, father.son$sheight, paired = TRUE)
"""
Paired t-test

data:  father.son$fheight and father.son$sheight
t = -11.789, df = 1077, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-1.1629160 -0.8310296
sample estimates:
mean of the differences 
-0.9969728 
"""

# test shows we should reject the null hypothesis and conclude that fathers and sons 
# (at least for this dataset) have different heights 

father.son$height_diff <- father.son$fheight - father.son$sheight

# visualise with density plot of diffences 
father.son %>% 
    ggplot2::ggplot(aes(x = height_diff)) +
    geom_density() +
    geom_vline(xintercept = mean(height_diff)) +
    geom_vline(xintercept = mean(height_diff) + 2 * c(-1, 1) * sd(height_diff) / sqrt(nrow(father.son)), 
               linetype = 2)

# see a distribution with mean not at zero and a confidence interval that barely excludes zero 
# which agrees with test

