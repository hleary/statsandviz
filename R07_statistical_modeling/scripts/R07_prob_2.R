# Problem Set: Statistical Modeling
# Question Number 2
# Status: Completed


# 2. Use the diminish.txt data (xv is explanatory, yv is response variable) to:
# a) Perform a simple linear regression
# b) Perform a polynomial (second-degree) regression
# c) Compare both models with Akaike’s Information Criterion (AIC). Which model is better?
# d) Make a scatterplot of the data and include both regression lines


# Libraries
library(tidyverse)   # data manipulation
library(car)         # regression
library(performance) # Check model assumptions
library(ggplot2)     # data viz

# Read in data
diminish <- read.csv(file = "R07_statistical_modeling/data/diminish.txt", sep = "\t", header = T)

# Simple linear regression
model_lm <- lm(yv ~ xv, data = diminish)
model_lm

# Polynomial (second-degree) regression
model_polynomial <- lm(yv ~ xv + I(xv^2), data = diminish)
model_polynomial

# AIC. Lower AIC is better model. 
AIC(model_lm)
AIC(model_polynomial) # best model

# Create a scatterplot of the data with regression lines
ggplot(data = diminish, aes(x = xv, y = yv)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(title = "Scatterplot of XV and YV",
       x = "XV",
       y = "YV")




