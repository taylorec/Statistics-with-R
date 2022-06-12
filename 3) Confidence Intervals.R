# Confidence Interval test for proportions
x <- 80; # number of successes from sample
n <- 125 # sample size
phat <- x/n
alpha <- 1 - 0.90
zstar <- qnorm(1 - alpha/2) # one-sided test
SE <- sqrt(phat * (1 - phat) / n)
MOE <- zstar * SE
phat + c(-1, 1) * MOE
prop.test(x, n) # compare to prop test
binom.test(x, n)$conf.int # show confidence interval

# Two-sided Confidence Interval test for mean
xbar <- 66; s <- 4; n <- 30
alpha <- 1 - 0.8 # 80% confidence level
tstar <- qt(1 - alpha/2, df = n-1)
SE <- s/sqrt(n)
MOE <- tstar * SE
xbar + c(-1,1) * MOE

# Two-sided Confidence Interval with given data
ozs <- c(1.95, 1.80, 2.10, 1.82, 1.75, 2.01, 1.83, 1.90)
qqnorm(ozs)
t.test(ozs, conf.level=0.90) # 90% CI
# value of 2 is not within this interval

# One-sided Confidence Interval with given data
x <- c(175, 185, 170, 184, 175)
t.test(x,conf.level = 0.90, alt="less")
# value 180 is within the interval

# Proportions testing for equality
# test a = 560 successes out of 1000 attempts and 
# test b = 570 successes out of 1200 attempts
prop.test(x=c(560, 570), n=c(1000,1200), conf.level=0.95)
# the interval misses including 0
# conclude that there appears to be a real difference in the population parameters


# Compare sample means
x <- c(0, 0, 0, 2, 4, 5, 13, 14, 14, 14, 15, 17, 17)
y <- c(0, 6, 7, 8, 11, 13, 16, 16, 16, 17, 18)
boxplot(list(placebo=x, ephedra=y), col="gray") # compare spreads
# means differ; medians are the same
t.test(x,y, var.equal=TRUE)
# the means are not equal