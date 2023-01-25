# Problem Set: Fundamentals of Statistics
# Problem Number 1
# Status: In Progress
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
r <- c(0.9, 1.4, 1.2, 1.2, 1.3, 2.0, 1.4, 1.6)


# Draw histogram
hist(r)

# Calculate sample mean
mean(r) # answer 1.375
avg <- mean(r)

# Calculate range
range(r) # answer 0.9 - 2.0

# Calculate standard deviation
sd(r) # answer 0.324
stdev <- sd(r)


# Write a function to express the standard deviation 
# as a percentage of the mean (that is, the coefficient of variation (CV)) and calculate it.
cv <- function(r){ sd (r)/mean(r)*100}
cv(r)
x<-c(1,2,3,4,5)
cv(x)
