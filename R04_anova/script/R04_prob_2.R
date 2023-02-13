# Problem Set: ANOVA
# Problem Number 2
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Data in growth.txt come from a farm-scale trial of animal diets. 
# There are two factors: diet and supplement. 
# Diet is a factor with three levels: barley, oats and wheat. 
# Supplement is a factor with four levels: agrimore, control, supergain and supersupp. 
# The response variable is weight gain after 6 weeks.
# 
# a) Inspect the data using boxplots
# b) Perform a two-way ANOVA
# c) Assess graphically if there exists an interaction between both factors
# d) Given what you learnt about the interaction, what would be a better model?
# e) What is the variation explained (R2) of this new model?
# f) Perform a Tukey HSD test of this new model

##########################################################################################################################



# =======================================================================================================================
# a) Inspect the data using boxplots ====================================================================================
# =======================================================================================================================

# Read in data
data <- read.table("C:\\Users\\hlear\\Desktop\\statsandviz\\R04_anova\\data\\growth.txt", sep = "\t", header = TRUE)

# Explore data
class(data)
head(data)
colnames(data)

# Create Boxplot
boxplot(gain~diet, data=data)             # weight gain as a function of diet
boxplot(gain~supplement, data=data)       # weight gain as a function of supplement
boxplot(gain~diet+supplement, data=data)  # weight gain for each combination of diet/supplement



# =======================================================================================================================
# b) Perform a two-way ANOVA ============================================================================================
# =======================================================================================================================

anova(lm(gain~diet+supplement, data=data))   # without interaction
summary(lm(gain~diet+supplement, data=data))$r.squared # for R-squared

anova(lm(gain~diet*supplement, data=data))   # includes interaction
summary(lm(gain~diet*supplement, data=data))$r.squared # for R-squared

# ======================================================================================================================
# For personal exploration: Akaike Information Criterion (AIC) assesses the best fit model =============================
# ======================================================================================================================

# install.packages("AICcmodavg")
# library(AICcmodavg)

# Fit the first linear regression model
two_way <- lm(gain ~ diet + supplement, data = data)

# Extract the AIC value for the first model
aic_two_way <- AIC(two_way)

# Fit the second linear regression model
interaction <- lm(gain ~ diet * supplement, data = data)

# Extract the AIC value for the second model
aic_interaction <- AIC(interaction)

# Compare the AIC values to determine which model is best
if (aic_two_way < aic_interaction) {
  cat("The two_way model is better, with AIC =", aic_two_way)
} else {
  cat("The interaction model is better, with AIC =", aic_interaction)
}

# two_way model is better


# =======================================================================================================================
# c) Assess graphically if there exists an interaction between both factors =============================================
# =======================================================================================================================

interaction.plot(data$diet, data$supplement, data$gain)  # supersupp and agrimore intersect at wheat 

interaction.plot(data$supplement, data$diet, data$gain)  # parallel (little to no interaction)



# =======================================================================================================================
# d) Given what you learnt about the interaction, what would be a better model? =========================================
# =======================================================================================================================

# The two-way (no interaction) model is better.



# =======================================================================================================================
# e) What is the variation explained (R2) of this new model? ============================================================
# =======================================================================================================================

anova(lm(gain~diet+supplement, data=data))    # without interaction
summary(lm(gain~diet+supplement, data=data))$r.squared  # for R-squared



# =======================================================================================================================
# f) Perform a Tukey HSD test of this new model =========================================================================
# =======================================================================================================================

TukeyHSD(aov(gain~diet+supplement, data=data))






