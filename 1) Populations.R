sample(0:1, size=10, replace=TRUE) # toss a coin 10 times

sample(1:6,size=10,replace=TRUE) # roll a die 10 times

## sum of roll of a pair of dice roll 10 times
sample(1:6,size=10,replace=TRUE) + sample(1:6,size=10,replace=TRUE)


## distributions
dunif(x=1, min=0, max=3) # 1/3 of area is to left of 1
punif(q=2, min=0, max=3) # 1/(b-a) is 2/3
qunif(p=1/2, min=0, max=3) # # half-way between
runif(n=1, min=0, max=3) # a random value in [0,3]

ps <- seq(0,1,by=.2) # probabilities
names(ps) <- as.character(seq(0, 100, by=20)) # give names
qunif(ps, min=0, max=1)

x <- runif(1000)
d <- density(x)
curve(dunif, -0.1,1.1, ylim=c(0, max(d$y,1)))
lines(d, lty=2)
rug(x)

# Bernoulli random variables
n <- 10; p <- 1/4
sample(0:1, size=n, replace=TRUE, prob=c(1-p,p))

# Toss a coin ten times. Let X be the number of heads.
dbinom(5, size=10, prob=1/2) # probability that X = 5
sum(dbinom(0:6, size=10, prob=1/2)) # probability that there are six or fewer heads
pbinom(6,size=10, p=1/2, lower.tail=FALSE) # probability of seven or more heads

# normal distribution 
pnorm(1, mean=0, sd=1) # z-score
qnorm(c(0.25, 0.5, 0.75)) # IQR
pnorm(1) - pnorm(-1) # area one standard deviation from mean
1 - 2*pnorm(-2) # area two standard deviations from mean
diff(pnorm(c(-3, 3))) # area three standard deviations from mean

# Central Limit Theorem
# The center stays the same, but as n gets bigger, 
# the spread of the mean gets smaller and smaller.
n <- 25; curve(dnorm(x, mean=0, sd=1/sqrt(n)), -3, 3, xlab="x", ylab="Densities of sample mean", bty="l")
n <- 5; curve(dnorm(x, mean=0, sd=1/sqrt(n)), add=TRUE)
n <- 1; curve(dnorm(x, mean=0, sd=1/sqrt(n)), add=TRUE)



