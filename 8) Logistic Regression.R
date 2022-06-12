# Spam email example
first.name <- gl(2, 2500, 5000, labels=c("yes", "no"))
offer <- gl(2, 1250, 5000, labels=c("yes", "no"))
opened <- c(rep(1:0, c(20, 1250-20)), rep(1:0, c(15, 1250-15)),
            rep(1:0, c(17, 1250-17)), rep(1:0, c(8, 1250-8)))
(xtabs(opened ~ first.name + offer))
res.glm <- glm(opened ~ first.name + offer, family="binomial")
summary(res.glm)
