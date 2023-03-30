# Problem Set: Statistical Modeling
# Question Number 10
# Status: Not completed


# 10. Data in Estuaries.csv correspond to counts of invertebrates 
# at 3-4 sites in each of 7 (randomly chosen) estuaries.

# Load Libraries
library(tidyverse)   # data manipulation
library(car)         # regression
library(performance) # check model assumptions
library(ggplot2)     # data viz
library(lme4)        # mixed effects models

# Load data
estuaries <- read.csv(file = "R07_statistical_modeling/data/Estuaries.csv", header = T)
head(estuaries)

# a) Fit a linear mixed model with Total as response and Modification as explanatory, controlling for Estuary
estuary.lme <- lmer(Total~Modification+(1|Estuary), data=estuaries)

# b) Estimate the R2 (conditional and marginal) of this model
r2(estuary.lme)

# c) Plot the data with ggplot2 in a way that helps you understand the different effects
# jitterplot with the effects of estuaries


# d) Include the variable Site as a random effect. 
# Do you think this corresponds to a crossed or a nested design? # Nested Design
# A few options. Others used a colon, but you can use a slash. 


# e) What are the R2 (conditional and marginal) of the model including Site


# f) Check the model assumptions
library(DHARMa)
simulateResiduals()

# g) Plot the data trying to include Site
ggplot(estuaries, aes(x=Site, y=Total, fill=Modification))+
  geom_boxplot()+
  geom_point()+
  facet_grid(~Estuary)

# h) Transform the variable Hydroid to presence/absence data
estuaries$Hydroid <- ifelse(estuaries$Hydroid==0, 0 , 1) # Not recommended since it changes the raw data

# i) Fit a generalized linear mixed model (GLMM) with this transformed variable as Response and the same fixed and random effects as in d). [Hint: function glmer]
hydroid.GLM <- glmer(Hydroid~Modification + (1|Estuary/Site), data = estuaries, family = "binomial")
summary(hydroid.GLM)

# j) Check the model assumptions
simulateResiduals(hydroid.GLM, plot = T)
r2(hydroid.GLM)

