# Problem Set: Fundamentals of Statistics
# Problem Number 2
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Blood pressure was measured (in units of mm Hg). Here are the measurements: 112, 128, 108, 129, 125, 153, 155, 132, 137.
#   a) How many individuals are in the sample?
#   b) What is the mean of this sample?
#   c) What is the variance?
#   d) What is the standard deviation?
#   e) What is the coefficient of variation?
  

##########################################################################################################################

# Assign blood pressure measurements (bp) to a vector
bp <- c(112, 128, 108, 129, 125, 153, 155, 132, 137)

# a) How many individuals are in the sample?
length(bp)

# b) What is the mean of this sample?
mean(bp)

# c) What is the variance?
var(bp)

# d) What is the standard deviation?
sd(bp)

# e) What is the coefficient of variation?
coef_var <- function(x) {
  sd(x) / mean(x) * 100
}
coef_var(bp)



