# Problem Set: Statistical Modeling
# Question Number 3
# Status: Completed


# 3. The data in stork.txt display the stress-induced corticosterone levels circulating 
# in the blood of European white storks and their survival over the subsequent five years 
# of study.

# a) Make a scatterplot of the data
# b) Which type of regression model is suitable for these data?
# c) Perform an appropriate regression to predict survival from corticosterone
# d) What is the pseudo-R2 of the model?
# e) What is the p-value of the model?
# f) Include the predicted curve in the scatterplot
# g) Check the model assumptions


# Libraries
library(tidyverse)   # data manipulation
library(car)         # regression
library(performance) # Check model assumptions
library(ggplot2)     # data viz
library(ggeffects)   # predicted probabilities


# Load in data
stork <- read.csv(file = "R07_statistical_modeling/data/stork.txt", sep = "\t", header = T)


# Scatterplot
plot(stork)

ggplot(stork, aes(x = Corticosterone, y = Survival)) +
  geom_point() +
  labs(title = "Scatterplot of Corticosterone and Survival",
       x = "Corticosterone (ng/ml)",
       y = "Survival (0 = Died, 1 = Survived)")

# Binary, so logistic regression is appropriate. 

# Logistic regression
model_glm <- glm(Survival ~ Corticosterone, data = stork, family = binomial)
model_glm

# Pseudo-R2 of the model
r2(model_glm)

# P-value of the model
summary(model_glm)

# Plot the predicted probabilities of survival
predicted <- ggpredict(model_glm, terms = "Corticosterone")
plot(predicted)

# Plot with logistic regression curve
ggplot(stork, aes(x = Corticosterone, y = Survival)) +
  geom_point() +
  labs(title = "Scatterplot of Corticosterone and Survival",
       x = "Corticosterone (ng/ml)",
       y = "Survival (0 = Died, 1 = Survived)") +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

# Model diagnostics
check_model(model_glm)
hist(resid(model_glm), main = "Histogram of Residuals", xlab = "Residuals")



