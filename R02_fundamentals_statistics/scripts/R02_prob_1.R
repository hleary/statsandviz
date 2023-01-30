# Problem Set: Fundamentals of Statistics
# Problem Number 1
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# A researcher videotaped the glides of 8 tree snakes leaping from a 10-m tower. 
# Undulation rates of the snakes measured in hertz (cycles per second) were as follows: 0.9, 1.4, 1.2, 1.2, 1.3, 2.0, 1.4, 1.6.

# a) Draw a histogram of the undulation rate
# b) Calculate the sample mean
# c) Calculate the range
# d) Calculate the standard deviation
# e) Write a function to express the standard deviation as a percentage of the mean (that is, the coefficient of variation) and calculate it.

##########################################################################################################################


# Put rates into a vector
und_rates <- c(0.9, 1.4, 1.2, 1.2, 1.3, 2.0, 1.4, 1.6)


# Draw histogram
hist(und_rates)

# Calculate sample mean
mean(und_rates) 

# Calculate range
range(und_rates) 

# Calculate standard deviation
sd(und_rates) 


# Write a function to express the standard deviation 
# as a percentage of the mean (that is, the coefficient of variation (CV)) and calculate it.
coef_var <- function(x) {
  sd(x) / mean(x) * 100
}
coef_var(und_rates) 




