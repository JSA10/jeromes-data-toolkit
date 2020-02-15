#Custom formulas

std_error <- function(s, n) {
    se <- s/(sqrt(n))
    return(se)
}

zscore <- function(mu, sig, xbar, n) {
    SE <- sig/sqrt(n)
    z <- (xbar - mu) / SE
    return(z)
}

# z and t are interchangeable in confidence interval calculations

lower_ci <- function (Xbar, z, std_error) {
    lci <- Xbar - (z * (std_error))
    return(lci)
}

upper_ci <- function (Xbar, z, std_error) {
    uci <- Xbar + (z * (std_error))
}

#rule of thumb #95% CI = 2 * se

t_statistic <- function(sm, pm, s, n) {
    meandiff <- sm - pm
    stan_error <- s/sqrt(n)
    t <- meandiff/stan_error
    return(t)
}

cohens_d <- function(sm, pm, s){
    d <- (sm-pm)/s
    return(d)
}

margin_error <- function(t, std_error) {
    m_error <- t * std_error
    return(m_error)
}


