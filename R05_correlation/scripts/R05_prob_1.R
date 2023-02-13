# Problem Set: Correlation
# Problem Number 1
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# A researcher investigated whether sound spectral properties of hyenas’ giggles are associated with age.
# Age(years): 2,2,2,6,9,10,13,10,14,14,12,7,11,11,14,20
# Frequency (Hz): 840,670,580,470,540,660,510,520,500,480,400,650,460,500,580,500
# a) Inspect the data using a scatterplot
# b) Test the linear association between both variables
# c) Assume that the data are not normally distributed, test the linear association using a non-parametric correlation coefficient

##########################################################################################################################


# Install packages and load libraries
install.packages("corrgram")
install.packages("cowplot")

library(tidyverse) # data wrangling and cleaning
library(ggplot2)   # data visualization
library(corrgram)  # graphical display of a correlation matrix (correlogram)
library(cowplot)   # creae publication-quality figures with ggplot2



# ===================================================================================================
# a) Inspect the data using a scatterplot ===========================================================
# ===================================================================================================

# Put the data into a data frame for manipulation
hyena <- data.frame(age = c(2,2,2,6,9,10,13,10,14,14,12,7,11,11,14,20), 
                    Hz = c(840,670,580,470,540,660,510,520,500,480,400,650,460,500,580,500))

# Explore data
class(hyena)
head(hyena)
colnames(hyena)

# Quick inspection of the data
# Plot using base R
plot(hyena)



# ===================================================================================================
# b) Test the linear association between both variables =============================================
# ===================================================================================================

# Fit the linear regression model
hyena_lm <- lm(Hz ~ age, data = hyena)
summary(hyena_lm) # to see R-squared and p-value

# Pairwise matrix using Pearson's Correlation Coefficient
# Pearson's r measures the strength and direction of the association.
cor(hyena, method = "pearson")

# Hypothesis test on the correlation coefficient
cor.test(hyena$age, hyena$Hz, method = "pearson")

# Plot using ggplot2
ggplot(data=hyena, aes(x=age, y=Hz)) +
  geom_point(size=2.5, alpha=0.4) +
  geom_smooth(method="lm", se=F, size=1.5, color="firebrick4") +
  xlab("Age (years)") +
  ylab("Giggle Frequency (Hz)") +
  ggtitle("Relationship between hyena age and giggle frequency (Hz)") +
  theme_cowplot(14)



# ==================================================================================================
# c) Test the linear association using a non-parametric correlation coefficient ====================
# ==================================================================================================

#Non-parametric rank correlation methods (Spearman, Kendall)
cor(hyena, method="spearman")
cor(hyena, method="kendall")



