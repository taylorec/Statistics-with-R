# Simulations
mu <- 100; sigma <- 16 # population parameters
x <- rnorm(16, mean=mu, sd=sigma) # sample
mean(x) # x bar

# Mutiple simulations
mu <- 100; sigma <- 16 # population parameters
M <- 4; n <- 16
res <- numeric(M)
for (i in 1:M) {
res[i] <- mean(rnorm(n, mean=mu, sd=sigma))
}
res

xbar <- function(i) mean(rnorm(n, mean=mu, sd=sigma)) # function
sapply(1:M, xbar)

# The central limit theorem through approximation
zstat <- function(x, mu, sigma) {
(mean(x) - mu) / (sigma/sqrt(length(x)))
}
M <- 2000; n <- 7
mu <- 100; sigma <- 16
res <- replicate(M, {
x <- rnorm(n, mean=mu, sd=sigma)
zstat(x, mu, sigma)
})
qqnorm(res, main="Normal, n=7")

# t-statistic
tstat <- function(x, mu) (mean(x) - mu) / (sd(x) / sqrt(length(x)))
mu <- 0; sigma <- 1
M <- 75; n <- 4
res <- replicate(M, tstat(rnorm(n, mu, sigma), mu))
boxplot(res)

## Estimation, confidence intervals

# Mean estimation
mu <- 100; sigma <- 16
M <- 1000; n <- 4
res <- replicate(M, {
x <- rnorm(n, mu, sigma)
SE <- sd(x)/sqrt(n) # standard error
(mean(x) - mu) / SE
})
quantile(res, c(0.025, 0.975))


p <- seq(0.05, 0.95, by=0.1) # possible values
prior <- c(2, 4, 8, 8, 4, 2, 1, 1, 1, 1) # how likely
prior <- prior / sum(prior) # as a probability
like <- p^11 * (1-p)^(27 - 11) # likelihood

