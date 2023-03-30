# Classroom Group Work
# Analyze "aphids" dataset

# Question and Hypothesis
# 


# Load Libraries
library(tidyverse)   # data manipulation
library(car)         # regression
library(performance) # check model assumptions
library(ggplot2)     # data viz
library(lme4)        # mixed effects models


# ================================================================ 
# STEP 1: LOAD DATA ==============================================
# ================================================================

aphids <- read.table(file = "R07_statistical_modeling/data/aphids.txt", sep = "\t", header = T)


# ================================================================
# STEP 2: EXPLORE DATA ===========================================
# ================================================================

aphids
head(aphids)
dim(aphids)

# Dataset is aphid counts in different plots and treatments over two seasons.
# "Plot" contains a unique identifier for each plot, ranging from 1 to 8. 
# "Treatment" specifies whether birds were present or excluded from the plot. 
# "Season" indicates which of the two seasons the observation was made in. 
# "Counts" gives the number of aphids counted in each plot.


# =================================================================
# STEP 3: VISUALIZE DATA ==========================================
# =================================================================

# Boxplot by treatment and season
ggplot(aphids, aes(x = Treatment, y = Counts, fill = Season)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Aphid counts", fill = "Season")

# Histogram of aphid counts
ggplot(aphids, aes(x = Counts)) +
  geom_histogram() +
  labs(x = "Aphid counts", y = "Frequency")

# Scatterplot of counts by plot
ggplot(aphids, aes(x = Plot, y = Counts)) +
  geom_point() +
  labs(x = "Plot", y = "Aphid counts")


# ================================================================
# STEP 4: STATISTICAL TESTS ======================================
# ================================================================

# ANOVA is used to determine if there is a significant difference 
# between the means of two or more groups. It assumes that the data 
# are normally distributed and that the variances of the groups are equal. 
# An ANOVA can be either one-way or two-way, depending on whether there is 
# one or two factors being studied.

# In the case of the aphids data, we have two treatments (present or excluded)
# and two seasons (Season1 or Season2), which suggests that an ANOVA may be 
# a more appropriate test than a t-test. Additionally, an ANOVA can help us 
# to test for interactions between the two factors (treatment and season), 
# which can provide more information about the effect of each factor on aphid 
# counts.

# Perform ANOVA
aphids.aov <- aov(Counts ~ Treatment * Season, data = aphids)
summary(aphids.aov)

# ======================================================================
# ANOVA Result Interpretation:
# "Treatment" does not have a significant effect on aphid count (p = 0.825)
# "Season" does have a significant effect on aphid count (p = 0.002)
# ======================================================================


# ======================================================================
# Diagnostics ==========================================================
# ======================================================================



# ======================================================================
# Communication ========================================================
# ======================================================================

# Count data (poisson) = glm
# Fixed: Treatment, Season
# Random: Plot
# so now lmer

