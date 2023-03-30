# Problem Set: Statistical Modeling
# Question Number 1
# Status: Completed


# 1. Use the ozone.txt data to model the ozone concentration
# as a linear function of wind speed, air temperature and the intensity of solar radiation. 
# Assume that the requirements to perform a linear regression are met.

# a) Make a multiple panel bivariate scatterplot
# b) Perform a multiple linear regression
# c) What is the variation explained, R2?
# d) Assess the co-linearity of the explanatory variables using the variance inflation factor
# e) Check the model assumptions


# Libraries
library(tidyverse)   # data manipulation
library(car)         # regression
library(performance) # Check model assumptions

# Read in the data
ozone <- read.csv(file = "R07_statistical_modeling/data/ozone.txt", sep="\t", header=T)

# Multiple panel bivariate scatterplot
pairs(ozone)

# Multiple linear regression
model_lm <- lm(ozone ~ wind + temp + rad, data = ozone)

# R^2
summary(model_lm)

# Co-linearity using variation inflation factor
# If any VIF values are >5, then there may be co-linearity issues.
vif(model_lm)

# Model diagnostics
check_model(model_lm)
hist(resid(model_lm), main = "Histogram of Residuals", xlab = "Residuals")




