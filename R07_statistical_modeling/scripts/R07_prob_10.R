# Problem Set: Statistical Modeling
# Question Number 10
# Status: Completed


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
ggplot(estuaries, aes(x = Modification, y = Total, color = Estuary)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3) +
  labs(title = "Total Invertebrates by Modification and Estuary")

ggplot(estuaries, aes(x = Modification, y = Total)) +  
  geom_boxplot()+  
  geom_jitter(aes(color=Estuary))


# d) Include the variable Site as a random effect. 
# Do you think this corresponds to a crossed or a nested design? 
# Fit the linear mixed model with Site as a random effect
model_site <- lmer(Total ~ Modification + (1 | Estuary) + (1 | Estuary:Site), data = estuaries)
    # Nested design because each site is nested within a specific estuary.


# e) What are the R2 (conditional and marginal) of the model including Site
r2(model_site)


# f) Check the model assumptions
library(DHARMa)
simulateResiduals(model_site, plot = T)

# g) Plot the data trying to include Site
ggplot(estuaries, aes(x=Site, y=Total, fill=Modification))+
  geom_boxplot()+
  geom_point()+
  facet_grid(~Estuary)

ggplot(estuaries, aes(x = Modification, y = Total, color = Estuary)) +
  geom_point(aes(shape = factor(Site)), size = 3) +
  labs(title = "Total Invertebrates by Modification, Estuary, and Site") +
  scale_shape_discrete(name = "Site")


# h) Transform the variable Hydroid to presence/absence data
estuaries$Hydroid <- ifelse(estuaries$Hydroid==0, 0 , 1) # Not recommended since it changes the raw data

# i) Fit a generalized linear mixed model (GLMM) with this transformed variable as Response and the same fixed and random effects as in d). [Hint: function glmer]
hydroid.GLM <- glmer(Hydroid~Modification + (1|Estuary/Site), data = estuaries, family = "binomial")
summary(hydroid.GLM)

# j) Check the model assumptions
simulateResiduals(hydroid.GLM, plot = T)
r2(hydroid.GLM)

