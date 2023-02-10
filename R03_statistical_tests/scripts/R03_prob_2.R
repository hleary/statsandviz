# Problem Set: Statistical Tests
# Problem Number 2
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# 2. The brown recluse spider often lives in houses throughout central North America. 
# A diet-preference study gave each of 41 spiders a choice between two crickets, one live and one dead. 
# 31 of the 41 spiders chose the dead cricket over the live one. 
# Does this represent evidence for a diet preference?
  
##########################################################################################################################


# Define the number of spiders and the number choosing the dead cricket
n <- 41
dead <- 31

# Binomial test
# Analyzing proportions with only 2 outcomes
binom.test(dead, n, p=0.5)

# Print the p-value
binom.test(dead, n, p=0.5)$p.value

# Does this represent evidence for a diet preference?
# Yes. The p-value is < 0.05, which means there is enough evidence to reject the null. 
