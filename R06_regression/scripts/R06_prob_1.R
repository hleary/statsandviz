# Problem Set: Regression
# Problem Number 1
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Testosterone is known to predict aggressive behavior 
# and is involved in face shape during puberty. 
# Does face shape predict aggression? 
# Researchers measured the face width-to-height ratio of 21 university hockey players 
# with the average number of penalty minutes awarded per game for aggressive infractions. 
# data are available in face.txt.

# a) How steeply does the number of penalty minutes increase per unit increase in face ratio? Calculate the estimate of the intercept. Write the result in the form of an equation for the line.
# b) How many degrees of freedom does this analysis have?
# c) What is the t-statistic?
# d) What is your final conclusion about the slope?
# e) What is the variation explained, R2?
# f) Make a scatterplot of the data and include a linear regression line
# g) Check the model assumptions

##########################################################################################################################



# Libraries
library(tidyverse)   # data wrangling and cleaning
library(ggplot2)     # data visualization
library(corrgram)    # graphical display of a correlation matrix (correlogram)
library(cowplot)     # creae publication-quality figures with ggplot2
library(performance) # assess model quality and goodness of fit
library(see)         # plotting, extras for ggplot2




# Read in the data
face_rawdata <- read.table(file = "data/face.txt", sep = "\t", header = TRUE)

# Explore data
class(face_rawdata)
colnames(face_rawdata)
head(face_rawdata)
nrow(face_rawdata)



# Quick scatterplot using base R
plot(face_rawdata)
plot(face_rawdata$Face, face_rawdata$Penalty) # Alternative code to assign x and y variables

# Linear model
face_lm <- lm(Penalty ~ Face, data=face_rawdata)
summary(face_lm)
abline(face_lm, col = "red", lwd = 3)  



# summary(face_lm) tells us answers to a-e.
# But we can extract each individually and print the answers... if we're so inclined:  

face_slope     <- round(coef(face_lm)[2],3)       
face_intercept <- round(coef(face_lm)[1],3)  
face_df        <- face_lm$df.residual
face_t_value   <- round(summary(face_lm)$coefficients[2, "t value"], 3)
face_p_value   <- round(summary(face_lm)$coefficients[2, "Pr(>|t|)"], 3)
face_r_squared <- round(summary(face_lm)$r.squared,3)


cat("Answer to questions:", "\n",
    "a) The equation is: Penalty =", face_intercept, "+", face_slope, "* Face", "\n",
    "b) Degrees of Freedom:", face_df, "\n",
    "c) t-value for Face variable:", face_t_value, " (p-value:", face_p_value, ")", "\n",
    "d) The slope of the relationship between Face and Penalty is positive and statistically significant.", "\n",
    "e) R-squared:", face_r_squared, "\n")



# Check model assumptions
check_model(face_lm)
