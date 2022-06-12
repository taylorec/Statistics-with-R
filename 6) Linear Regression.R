library(ggplot2)
# ?mtcars #mtcars dataset information
head(mtcars)
any(is.na(mtcars))
str(mtcars)

cor(mtcars) #strongest positive/negative correlations with mpg are wt, cyl, disp

qplot(mpg, disp, data=mtcars)

pl <- ggplot(data=mtcars,aes(x = wt,y=mpg)) 
pl + geom_point(aes(color=factor(cyl)),size=4,alpha=0.6)

pl <- ggplot(mtcars, aes(factor(cyl), mpg))
pl + geom_boxplot(aes(fill = factor(cyl)))

g <- ggplot(mpg, aes(class))
g + geom_bar(aes(fill = drv), position = "dodge")

# Simple Linear Regression Model
model <- lm(mtcars$mpg~mtcars$disp) # linear model of mpg predicted on displacement
plot(y=mtcars$mpg, x=mtcars$disp, xlab='Engine Size(cubic inches)',
		ylab='Fuel Efficiency (Miles per Gallon)', main='Fuel Efficiency')
abline(a=coef(model[1]), b=coef(model[2]), lty=2)
summary(model) 


# Multivariate Regression Model
lm.mr1 <- lm(mpg~disp+wt+cyl, data=mtcars)
summary(lm.mr1) # disp is not significant with mulitple predictors

lm.mr2 <- lm(mpg~wt+cyl, data=mtcars)
summary(lm.mr2) # both predictor variables are significant; Adjusted R-squared: 0.8185 
resid <- as.data.frame(residuals(lm.mr2))
plot(resid)

# Lasso Regression Model to eliminate non-significant variables
library(lasso2)
lm.lasso <- l1ce(mpg~., data=mtcars)
summary(lm.lasso)$coefficients; # eliminate zero coefficient variables
lm.lasso2 <- l1ce(mpg~cyl+hp+wt+am+carb, data=mtcars)
summary(lm.lasso)$coefficients # eliminate zero coefficient variables
lm.lasso3 <- l1ce(mpg~cyl+hp+wt, data=mtcars)
summary(lm.lasso3)$coefficients # eliminate zero coefficient variables
lm.lasso4 <- l1ce(mpg~cyl+wt, data=mtcars)
summary(lm.lasso4)$coefficients # same variables as lm.mr2