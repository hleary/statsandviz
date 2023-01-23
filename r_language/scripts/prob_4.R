# Problem Set: R Language
# Problem Number 4
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Write a function (sum_n) that for any given value, say n, computes the sum of the integers from 1 to n (inclusive).
# Use the function to determine the sum of integers from 1 to 5,000.

###########################################################################################################################


# See lines 268 - 284 in the R01_RLanguage script


# Write a function that computes the sum of integers from 1 to n (inclusive).
sum_n <- function(n){sum(1:n)}


# Quick test with n = 5. 
1+2+3+4+5 # Run this to see what the sum of integers from 1 to 5 equals. Answer: 15 
sum_n(5) # Now test the function with n = 5. Answer: 15


# Use the function to determine the sum of integers from 1 to 5,000.
sum_n(5000)
