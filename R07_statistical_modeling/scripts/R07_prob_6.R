# Problem Set: Statistical Modeling
# Question Number 6
# Status: Completed


# 6. In a recent paper we read: 
# “Linear mixed effects modelling fit by restricted maximum likelihood was used 
# to explain the variations in growth. The linear mixed effects model was generated 
# using the lmer function in the R package lme4, with turbidity, temperature, tide, 
# and wave action set as fixed factors and site and date set as random effects”. 

# Write down the R code to recreate their model.

# Load libraries
library(lme4)

# Load data into dataframe. Let's call it "data"...
data <- read.table("data.txt", sep = "\t", header = T)

# Generate the linear mixed effects model 
# with turbidity, temperature, tide, and wave action as fixed factors
# and site and date as random effects
model <- lmer(growth ~ turbidity + temperature + tide + wave_action + (1 | site) + (1 | date), data = data)

# Output
summary(model)


