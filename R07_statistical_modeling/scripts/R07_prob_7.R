# Problem Set: Statistical Modeling
# Question Number 7
# Status: Completed


# 7. Researchers at the University of Arizona want to assess the germination rate 
# of saguaros using a factorial design, 
# with 3 levels of soil type (remnant, cultivated and restored) 
# and 2 levels of sterilization (yes or no). 
# The same experimental design was deployed in 4 different greenhouses. 
# Each of the unique treatments was replicated in 5 pots. 6 seeds planted in each pot.

# a) How many fixed treatments (unique combinations) exist?
# b) What is the total number of pots?
# c) What is the total number of plants measured?
# d) Write the R code that you would use to analyze these data 


# a) 
# 3 soil types and 2 levels of sterilization 
# 3 x 2 = 6 fixed treatments

# b) 
# 6 fixed treatments, Replicated in 5 pots, 4 different greenhouses 
# 6 x 5 x 4 = 120 total number of pots

# c) 
# 6 seeds planted in each pot, 120 pots in total 
# 6 x 120 = 720 total number of plants.

# d) 
# R code to analyze these data:

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

# Create a data frame with the experimental design
saguaros <- expand.grid(soil_type = c("remnant", "cultivated", "restored"),
                      sterilization = c("yes", "no"),
                      greenhouse = 1:4,
                      pot = 1:5)

# Simulate germination data for each treatment
set.seed(123)
germination <- data.frame(treatment = 1:(nrow(saguaros)),
                          germination = rbinom(nrow(saguaros), 6, 0.5))

# Combine the design and germination data frames
data <- cbind(saguaros, germination)


# Fit the mixed-effects logistic regression model
mixed_effects <- glmer(germination ~ soil_type * sterilization + (1 | greenhouse/pot),
                       data = data, family = binomial())

# Summarize the results
summary(mixed_effects)



