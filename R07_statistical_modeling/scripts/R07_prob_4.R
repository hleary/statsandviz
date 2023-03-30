# Problem Set: Statistical Modeling
# Question Number 4
# Status: Completed


# 4. The clusters.txt dataset contains the response variable Cancers (cases per year per clinic) 
# and the explanatory variable Distance (the distance from a nuclear plant to the clinic in kilometers).

# a) Make a scatterplot of the data
# b) Which regression is the more appropriate for these data? (Don’t take overdispersion into account for now)
# c) Given your choice, is the trend significant?
# d) What is the pseudo-R2 of the model?
# e) What is the p-value of the model?
# f) Include the predicted relationship from the model in the scatterplot
# g) Do you think there might be some evidence of overdispersion?
# h) Perform a new generalized linear model with a distribution that better accounts for overdispersion
# i) Check this last model assumptions


# Libraries
library(tidyverse)   # data manipulation
library(car)         # regression
library(performance) # check model assumptions
library(ggplot2)     # data viz
library(ggeffects)   # predicted probabilities
library(MASS)        # perform negative binomial regression


clusters <- read.csv(file = "R07_statistical_modeling/data/clusters.txt", sep = "\t", header = T)

# Make a scatterplot of the data
ggplot(clusters, aes(x=Distance, y=Cancers)) + geom_point()

# Which regression is the more appropriate for these data?(Don’t take overdispersion into account for now)
# Since response variable is a count, a Poisson regression model is appropriate.

# Given your choice, is the trend significant?
# Fit a Poisson regression model
model_poisson <- glm(Cancers ~ Distance, data=clusters, family=poisson)
summary(model_poisson)
# p-value 0.0941 > 0.05 ; not significant

# What is the pseudo-R2 of the model?
r2(model_poisson)

# Include the predicted relationship from the model in the scatterplot
ggplot(clusters, aes(x=Distance, y=Cancers)) + 
  geom_point() + 
  geom_smooth(method="glm", method.args=list(family="poisson"), se=FALSE)

# Do you think there might be some evidence of overdispersion?
check_overdispersion(model_poisson)

# Perform a new generalized linear model with a distribution that better accounts for overdispersion
# Negative binomial regression model
model_nb <- glm.nb(Cancers ~ Distance, data=clusters)
summary(model_nb)

# Check this last model assumptions
# Model diagnostics
check_model(model_nb)
hist(resid(model_nb), main = "Histogram of Residuals", xlab = "Residuals")


