# simulate multinomial data
cols <- c("blue", "brown", "green", "orange", "red", "yellow", "purple")
prob <- c(1, 1, 1, 1, 2, 2, 2) # ratio of colors
prob <- prob / sum(prob)
n <- 30
bagful <- sample(cols, n, replace=TRUE, prob=prob)
table(bagful)

# chi-square test
y <- c(35, 40, 25)# actual values
p <- c(35, 35, 30) # expected ratios
p <- p/sum(p) # expected proportions
chisq.test(y, p=p)

## Chi-square test of independence
seatbelt <- rbind(c(56,8), c(2,16))
seatbelt
chisq.test(seatbelt)
# the two variables are not independent

## The chi-squared test of homogeneity
celexa <- c(2, 3, 7)
placebo <- c(2, 8, 2)
x <- rbind(celexa, placebo)
colnames(x) <- c("worse", "same", "better")
x
chisq.test(x)
chisq.test(x, simulate.p.value=TRUE)

