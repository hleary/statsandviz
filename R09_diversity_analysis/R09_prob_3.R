# Problem Set: Diversity Analysis
# Question Number 3
# Status: Completed


# 3. Write a function to calculate the Shannon-Wiener diversity index 
# of a vector representing the abundances of different species 
# in a community.

shannon_diversity <- function(abundances) {
  abundances <- abundances[abundances > 0]  # Filter out zero abundance values
  prop <- abundances / sum(abundances)  # Calculate proportion of each species
  log_prop <- log(prop)  # Take the natural logarithm of each proportion
  -sum(prop * log_prop)  # Calculate the Shannon-Wiener index
}

abundances <- c(5, 2, 0, 1, 4, 5, 3)
shannon_diversity(abundances)
