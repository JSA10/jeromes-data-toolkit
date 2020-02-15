# causal impact model template
# - using vignette: https://google.github.io/CausalImpact/CausalImpact.html

#install.packages("CausalImpact")
library(CausalImpact)


# create example time-series dataset --------------------------------------------------

set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

# simple dataset - y (response variable) and x (a control or in this case predictor)
# - in practise would have more predictors
# 100 observation time-series
# created impact by adding 10 to all y values from row 70 upwards

str(data)
head(data)
summary(data)

# visualise with matrices plot
matplot(data, type = "l")


# analyse -----------------------------------------------------------------

# PROCESS
# 1) specify period used for training model (pre-intervention) and period for
#   computing the 'counterfactual' prediction (post-intervention)
# 2) Run inference using CausalImpact function, with your data and periods as inputs
# 3) Visualise and analyse the CausalImpact object (using plot and summary)


pre.period <- c(1, 70)
post.period <- c(71, 100)


impact <- CausalImpact(data, pre.period, post.period)
# this assembles a structural time-series model, performs posterior inference
# computes estimates of the causal effect.
# RETURNS A CausalImpact object



# visualise CausalImpact object -------------------------------------------

# simplest way is with plot function that's been adapted for this package
plot(impact)

# defaults to 3 panels
# 1 - shows Y variable over time compared to the models counterfactual prediction
# 2 - shows the difference between observed data and counterfactual predictions (pointwise causal effect)
# 3 - shows the cumulative effect by adding up the pointwise causal effects from 2


# ASSUMPTIONS -------------------------------------------------------------


"""
Remember, once again, that all of the above inferences depend critically on the
assumption that the covariates were not themselves affected by the intervention.

The model also assumes that the relationship between covariates and treated time
series, as established during the pre-period, remains stable throughout the
post-period.
"""


# Using time-series objects -----------------------------------------------

# often simpler to use time-series objects rather than a data frame

# example data
time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(y, x1), time.points)
head(data)

# can now specify using time points rather than row indices
pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

impact <- CausalImpact(data, pre.period, post.period)

#this means the plots show dates rather than indices
plot(impact)


# Summarise ---------------------------------------------------------------


summary(impact)
# average col shows average impact across all post period points
# looking for value for average absolute effect (11) and related credible interval (9.8 - 11)
# since this excludes 0 we can corectly assume an impact was had
# (and we know it's close to the value of 10 we inputted)

"""
In reality need to gauge how likely the predictor variable is to be distinct from
the intervention
"""

# a verbose report output is available
summary(impact, "report")

# extract values from summary table at full precision
impact$summary
s

# see vignette for custom models ------------------------------------------
#https://google.github.io/CausalImpact/CausalImpact.html


