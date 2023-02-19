#####################################################################################
## 06. Regression
#####################################################################################

#REGRESSION: predicts the value of one variable from that of another.
#y (response or dependent variable)
#x (explanatory or independent variable)

#Linear regression: y = b*x + a (b=slope; a=intercept)
fit <- lm(weight ~ height, data=women)
summary(fit) #for R^2 and p-value of the model

plot(women$height,women$weight, xlab="Height (in inches)", ylab="Weight (in pounds)")
abline(fit, col="red", lwd=3)
abline(a=-87.5, b=3.45)

install.packages('ggplot2')
library(ggplot2) #using ggplot2 defaults
ggplot(aes(x=height, y=weight), data=women)+
  geom_point()+
  geom_smooth(method="lm", se=F)

#Regression diagnostics
par(mfrow=c(2,2))
plot(fit)
layout(1) #lm is not a good model

hist(resid(fit)) # extracts residuals (we would expect a normal distribution, but nope)
plot(women$height, resid(fit)) # if no pattern, good fit; if pattern, poor fit

library(gvlma) #package for global validation
summary(gvlma(fit))

library(performance) #this package is very visual and extremely helpful to check model assumptions
install.packages('see')
library(see)
check_model(fit)
check_posterior_predictions(fit) #posterior predictive checks
# bayesian


