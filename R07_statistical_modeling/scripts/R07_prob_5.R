# Problem Set: Statistical Modeling
# Question Number 5
# Status: Completed


# 5. Use the jaws.txt data to:
# a) Make a scatterplot of the data (age explanatory, bone response)
# b) Perform a non-linear regression assuming an asymptotic exponential relationship:
#   y=a(1-e^(-cx))
# c) Perform a non-linear regression assuming a Michaelis-Menten model:
#   y=ax/(1+bx)
# d) Estimate the percentage of variation explained by both models (comparing them with a null model with only a constant) 
# e) Compare both models with Akaike’s Information Criterion (AIC). Which model is better?
# f) Make a scatterplot of the data and include both regression lines


# Libraries
library(tidyverse)   # data manipulation
library(car)         # regression
library(performance) # check model assumptions
library(ggplot2)     # data viz
library(ggeffects)   # predicted probabilities
library(MASS)        # perform negative binomial regression


# Read in data
jaws <- read.csv(file = "R07_statistical_modeling/data/jaws.txt", sep = "\t", header = T)

# a) Make a scatterplot of the data (age explanatory, bone response)
ggplot(jaws, aes(x = age, y = bone)) +
  geom_point() +
  labs(x = "Age (years)", y = "Bone length (mm)",
       title = "Scatterplot of age vs. bone length")


# b) Perform a non-linear regression assuming an asymptotic exponential relationship:
#   y=a(1-e^(-cx))

# "a" is the estimated maximum bone length
range(jaws$bone) # 142

# Regression to get the "c" starting value
jaws_lm <- lm(bone ~ age, data = jaws)
summary(jaws_lm) # slope = 1.642

# The starting value for parameter c is estimated by taking the reciprocal 
# of the slope of the linear regression between age and bone length.
# c = 1/1.642 = 0.6088
exp_model <- nls(bone ~ a*(1-exp(-c*age)), data = jaws, start = list(a = 142, c = 0.6088))
summary(exp_model)

# c) Perform a non-linear regression assuming a Michaelis-Menten model:
#   y=ax/(1+bx)
mm_model <- nls(bone ~ a*age/(1+b*age), data = jaws, start = list(a = 142, b = 0.6088))
summary(mm_model)


# d) Estimate the percentage of variation explained by both models 
# (comparing them with a null model with only a constant) 
null_model <- lm(bone ~ 1, data = jaws)
1 - (sum(residuals(exp_model)^2)/sum(residuals(null_model)^2))
1 - (sum(residuals(mm_model)^2)/sum(residuals(null_model)^2))

# e) Compare both models with Akaike’s Information Criterion (AIC). 
# Which model is better?
AIC(exp_model)
AIC(mm_model) # higher AIC

# f) Make a scatterplot of the data and include both regression lines
ggplot(jaws, aes(x = age, y = bone)) +
  geom_point() +
  labs(x = "Age (years)", y = "Bone length (mm)", 
       title = "Scatterplot of age vs. bone length") +
  geom_smooth(method = "nls", 
              formula = y ~ a*(1-exp(-c*x)), 
              se = FALSE, 
              method.args = list(start = c(a = 140, c = 0.1)), 
              color = "red") +
  geom_smooth(method = "nls", 
              formula = y ~ a*x/(1+b*x), 
              se = FALSE, 
              method.args = list(start = c(a = 100, b = 0.01)), 
              color = "blue") +
  theme_bw()


# Alternative code
# f) Make a scatterplot of the data and include both regression lines

# Create predicted values for both models
jaws$exp_pred <- predict(exp_model)
jaws$mm_pred <- predict(mm_model)

# Create scatterplot with both regression lines
ggplot(jaws, aes(x = age, y = bone)) +
  geom_point() +
  geom_line(aes(y = exp_pred), color = "blue") +
  geom_line(aes(y = mm_pred), color = "red") +
  labs(x = "Age (years)", y = "Bone length (mm)",
       title = "Regression lines for age vs. bone length",
       color = "Model") +
  scale_color_manual(values = c("blue", "red"), labels = c("Exponential", "Michaelis-Menten"))






