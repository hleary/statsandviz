# Problem Set: R Language
# Problem Number 5
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Write a function (sqrt_round) that takes a number x as input, 
# takes the square root, 
# rounds it to the nearest whole number 
# and then returns the result.

###########################################################################################################################

sqrt_round <- function(x) {
  result <- round(sqrt(x))
  return(result)
}


# Test
sqrt(17)
sqrt_round(17)
