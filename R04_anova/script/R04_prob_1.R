# Problem Set: ANOVA
# Problem Number 1
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# You are hired by the US Department of Agriculture to develop effective control practices for invasive plants (data_ANOVA.xlsx). 
# In particular, you are asked to investigate pesticide controls for kudzu which is an invasive vine 
# that grows in thick mats that smother underlying plants. 
# Two of the most widely used pesticides for kudzu are glyphosate and triclopyr. 
# 
# To determine the effectiveness of a single application of pesticide, 
# you conduct an experiment in 18 plots that each had 50% kudzu cover. 
# In mid-summer, you applied equal amounts of 2% glyphosate to 6 plots, 2% triclopyr to 6 plots, and water without pesticides to 6 plots. 
# Then you returned in autumn and measured the percent cover of kudzu in the plots.

# a) Transform the data from wide to long format (Google how to do it - Hint: function melt from reshape2 package)
# b) Make a boxplot of the data
# c) Perform an ANOVA
# d) What is the variation explained (R-squared)?
# e) Perform a post hoc test

##########################################################################################################################



# Install and load packages
# install.packages('reshape2')
# library(reshape2)
# library(readxl)



# =======================================================================================================================
# a) Transform the data from wide to long format ========================================================================
# =======================================================================================================================

# Read in  wide format data
data_wide <- read_xlsx("C:\\Users\\hlear\\Desktop\\statsandviz\\R04_anova\\data\\data_ANOVA.xlsx")

# Explore data
head(data_wide)
colnames(data_wide)
class(data_wide)

# Melt the data into long format
data_long <- melt(data_wide, id.vars = NULL)

# Explore data
data_long
colnames(data_long)
class(data_long)



# =======================================================================================================================
# b) Make boxplot =======================================================================================================
# =======================================================================================================================

ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  ggtitle("Percent Cover of Kudzu After Treatment") +
  xlab("Treatment") +
  ylab("Percent Cover")



# ======================================================================================================================
# c) Perform an ANOVA ==================================================================================================
# ======================================================================================================================

# Fit a linear regression model
model <- lm(value ~ variable, data = data_long)

# Perform an ANOVA on the model
anova(model)

# In this output, the p-value is very small (1.318e-07), 
# which suggests that there is a significant difference between the means of the groups. 
# The "***" sign next to the p-value indicates that the results are highly significant (p < 0.001).



# ======================================================================================================================
# d) What is the variation explained (R-squared)? ======================================================================
# ======================================================================================================================

summary(model)$r.squared

# R-squared value indicates that the regression model fits the data well



# ======================================================================================================================
# e) Perform a post hoc test============================================================================================
# ======================================================================================================================

# Perform Tukey's test
TukeyHSD(aov(value~variable, data=data_long))




