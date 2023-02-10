# Problem Set: Statistical Tests
# Problem Number 4
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# A bee biologist is analyzing whether there was an association between bee colony number and type of forest habitat. 
# We expect that there is no habitat preference for bee colony number. 
# Is this true based on this data?
# 
# a) Make a barplot of the data
# b) Test the hypothesis of no-association
# 
# Habitat	Bee colonies
# Oak	33
# Hickory	30
# Maple	29
# Red cedar	4
# Poplar	4

##########################################################################################################################

# Put the data into a vector
data <- c("Oak"=33, "Hickory"=30, "Maple"=29, "Red Cedar"=4, "Poplar"=4)

# Barplot
barplot(data)

# Test hypothesis of no-association
# Chi-squared goodness-of-fit test:
# Measures the discrepancy between an observed frequency distribution and the frequencies expected under a random model
chisq.test(data)

# Show p-value
chisq.test(data)$p.value

# The p-value is > 0.05, suggesting there is evidence for habitat preference for bee colony number.
