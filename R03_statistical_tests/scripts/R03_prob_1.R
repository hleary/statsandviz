# Problem Set: Statistical Tests
# Problem Number 1
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# 1. Normal human body temperature is 98.6 F. 
# Researchers obtained body-temperature measurements on randomly chosen healthy people: 98.4, 98.6, 97.8, 98.8, 97.9, 99.0, 98.2, 98.8, 98.8, 99.0, 98.0, 99.2, 99.5, 99.4, 98.4, 99.1, 98.4, 97.6, 97.4, 97.5, 97.5, 98.8, 98.6, 100.0, 98.4.
# a) Make a histogram of the data
# b) Make a normal quantile plot
# c) Perform a Shapiro-Wilk test to test for normality
# d) Are the data normally distributed?
# e) Are these measurements consistent with a population mean of 98.6 F?

##########################################################################################################################


# Put body temp values into a vector
temp <- c(98.4, 98.6, 97.8, 98.8, 97.9, 99.0, 98.2, 98.8, 98.8, 99.0, 98.0, 99.2, 99.5, 99.4, 98.4, 99.1, 98.4, 97.6, 97.4, 97.5, 97.5, 98.8, 98.6, 100.0, 98.4)

# a) Histogram 
hist(temp)

# b) Normal Quantile Plot
qqnorm(temp)
qqline(temp)

# c) Shapiro-Wilk Test
shapiro.test(temp)

# d) Are the data normally distributed?
# Yes. Points follow the QQ line closely, indicating the data follows a normal distribution.
# Also, the Shapiro-Wilk test p-value > 0.05, which means we fail to reject the null.

# e) Are these measurements consistent with a population mean of 98.6 F? 
t.test(temp, mu=98.6)
# Yes, the measurements are consistent with a mean of 98.6F. 
# The p-value of 0.5802 is > 0.05, and 98.6 falls within the confidence intervals of the mean.
# We fail to reject the null.
