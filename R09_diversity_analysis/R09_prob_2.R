# Problem Set: Diversity Analysis
# Question Number 2
# Status: Completed


# 2. Write a function to calculate the observed richness of a 
# vector representing the abundances of different species in a community.

richness <- function(abundances) {
  sum(abundances > 0)
}


# Test
abundances <- c(0, 2, 0, 1, 4, 0, 3) # abundances of diff species

richness(abundances) # four species with >0 abundances
