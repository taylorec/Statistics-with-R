## one-way ANOVA
# random sample of calorie intake for the given month
may <- c(2166, 1568, 2233, 1882, 2019)
sep <- c(2279, 2075, 2131, 2009, 1793)
dec <- c(2226, 2154, 2583, 2010, 2190)
d <- stack(list(may=may, sep=sep, dec=dec)) # need names for list
names(d)
oneway.test(values ~ ind, data=d, var.equal=TRUE)
res <- aov(values ~ ind, data = d)
summary(res)
# p-value is high; this test is not significant and fails
# to conclude that at least one mean is significantly different 

# ANOVA for categorical data
UBP <- c(168.2, 161.4, 163.2, 166.7, 173.0, 173.3, 160.1, 161.2, 166.8)
grip.type <- gl(3, 3, 9, labels=c("classic", "integrated", "modern"))
grip.type
boxplot(UBP ~ grip.type, ylab="Power (watts)", main="Effect of cross country grip")
# integrated has the highest mean shown by the boxplot
res <- aov(UBP ~ grip.type)
summary(res)
# p-value is significant at the 10% level

# two-way ANOVA
d <- data.frame(
# Amount of high-energy drink consumed
x =c(0, 0, 16, 15, 14, 11, 26, 27, 23, 23, 36, 43), 
# whether parent is satisfied with the number of shared evening meals
   satisfied=c("yes", "no")[c(1,1,2,2,2,1,2,2,1,1,2,2)],
# amount of television watched
   tv=rep(c("0-1", "1-5", "5+"), c(5, 3, 4))
)
d
res.add <- lm(x ~ tv + satisfied, data=d)
res.int <- lm(x ~ tv * satisfied, data=d)
summary(res.int)
with(d, interaction.plot( tv, satisfied, x)) # interaction plot
summary(res.add)
anova(res.int, res.add) # partial F-test




