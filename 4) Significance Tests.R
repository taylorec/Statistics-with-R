# Significance test for a population proportion
# H0: p = 0.1500, HA: p > 0.1500 - one-sided test
phat <- 22695 / 15000
p0 <- 0.1500; n <- 15000
SD <- sqrt(p0 * (1-p0)/n)
pnorm(phat, mean=p0, sd=SD, lower.tail=FALSE)
prop.test(x=22695, n=150000, p=0.15, alternative="greater")
# not statistically significant at alpha=0.05

# HA : p /= 0.1500 - two-sided test
prop.test(x=22695, n=150000, p=.1500, alternative="two.sided")


# Significance test for a population mean
# H0: M = 17.0, HA: M < 17 - one-sided test
mpg <- c(11.4, 13.1, 14.7, 14.7, 15.0, 15.5, 15.6, 15.9, 16.0, 16.8)
xbar <- mean(mpg); s <- sd(mpg); n <- length(mpg)
c(xbar=xbar, s=s, n=n)
SE <- s/sqrt(n)
obs <- (xbar - 17)/SE
pt(obs, df = 9, lower.tail = T)
# The p-value is very small and discredits the claim of 17 miles per gallon
t.test(mpg, mu = 17, alternative="less")

# Significance test for population mean
costs <- c(304, 431, 385, 987, 303, 480, 455, 724, 642, 506)
# are costs greater than 500?
t.test(costs, mu=500, alternative='greater')
# The p-value is not small. The data gives little reason to doubt the
# null hypothesis applies to this data.

# Perform a Power test to return needed min sample size
alpha <- 0.05; beta = 0.20
power.t.test(delta=1, sd=1, sig.level=alpha, power=1-beta,
     type="one.sample", alternative="one.sided")


## Two-sample tests of proportion
phat <- c(.1500, .1513) # the sample proportions
# H0: p1 = p2, HA: p1 < p2.
n <- c(16000, 15000) # the sample sizes
n * phat # counts
prop.test(n * phat, n, alternative="less")
# difference is not statistically significant

## Two-sample tests of center
m300 <- c(284, 279, 289, 292, 287, 295, 285, 279, 306, 298)
m600 <- c(298, 307, 297, 279, 291, 335, 299, 300, 306, 291)
# H0: µx = µy, HA: µx /= µy
plot(density(m300))
lines(density(m600), lty=2)
t.test(m300, m600, var.equal=TRUE)
# p-value is 0.05696 for the two-sided test
# This suggests a difference in the mean values, 
# but it is not statistically significant

# Matched samples
Finasteride <- c(5, 3, 5, 6, 4, 4, 7, 4, 3)
placebo <- c(2, 3, 2, 4, 2, 2, 3, 4, 2)
t.test(Finasteride, placebo, paired=TRUE, alternative="two.sided")

 


