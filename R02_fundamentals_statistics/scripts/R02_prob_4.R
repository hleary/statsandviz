# Problem Set: Fundamentals of Statistics
# Problem Number 4
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Calculate the probability of each of the following events:
# a) A standard normally distributed variable is larger than 3
# b) A normally distributed variable with mean 35 and standard deviation 6 is larger than 42
# c) Getting 10 out of 10 successes in a binomial distribution with probability 0.8
# d) X > 6.5 in a Chi-squared distribution with 2 degrees of freedom

##########################################################################################################################


# a) A standard normally distributed variable is larger than 3
pnorm(3, lower.tail = FALSE)

# b) A normally distributed variable with mean 35 and standard deviation 6 is larger than 42
pnorm(42, mean = 35, sd = 6, lower.tail = FALSE)

# c) Getting 10 out of 10 successes in a binomial distribution with probability 0.8
dbinom(10, size = 10, prob = 0.8)

# d) X > 6.5 in a Chi-squared distribution with 2 degrees of freedom
pchisq(6.5, df = 2, lower.tail = FALSE)
