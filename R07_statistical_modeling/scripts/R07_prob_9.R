# Problem Set: Statistical Modeling
# Question Number 9
# Status: Completed



# Load data
dragons <- load("R07_statistical_modeling/data/dragons.RData")

# Explore data
print(dragons)    # display names of objects that were loaded
str(dragons)      # structure
summary(dragons)  # summary


# a) Perform a simple linear regression with testScore as response and bodyLength as explanatory
dragons_lm <- lm(testScore ~ bodyLength, data = dragons)
summary(dragons_lm)


# b) Plot the data with ggplot2 and add a linear regression line with confidence intervals. Hint: geom_smooth()
library(ggplot2)

ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)


# c) We collected multiple samples from 8 mountain ranges. Generate a boxplot using (or not) ggplot2 to explore this new explanatory variable
ggplot(dragons, aes(x = mountainRange, y = testScore)) +
  geom_boxplot()


# d) Now repeat the scatterplot in b) but coloring by mountain range and without linear regression line
ggplot(dragons, aes(x = bodyLength, y = testScore, color = mountainRange)) +
  geom_point()


# e) Instead of coloring, use facet_wrap() to separate by mountain range
ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point() +
  facet_wrap(~ mountainRange)


# f) Perform a new linear model adding mountain range and assuming that it is fixed effects
dragons_lm_fixed <- lm(testScore ~ bodyLength + mountainRange, data = dragons)
summary(dragons_lm_fixed)


# g) Perform the same linear model as before but now assuming mountain range is random effects
library(lme4)

dragons_lm_random <- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragons)
summary(dragons_lm_random)


# h) How much of the variation in test scores is explained by the random effect (mountain range)
VarCorr(dragons_lm_random)


# i) Estimate the R2 (conditional and marginal) of the mixed-effects model
library(performance)

performance::r2(dragons_lm_random)


# j) Check this last model assumptions
library(DHARMa) #model diagnostics for glm
simulateResiduals(dragons_lm_random, plot = T)

# k) Include the fitted lines for each mountain range (plot from e). [Hint: predict function within geom_line layer]
dragons$predicted <- predict(dragons_lm_random, re.form = NA)

ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point() +
  geom_line(aes(y = predicted, group = mountainRange), color = "blue") +
  facet_wrap(~ mountainRange)

