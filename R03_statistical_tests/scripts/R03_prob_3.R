# Problem Set: Statistical Tests
# Problem Number 3
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Ten epileptic patients participated in a study of a new anticonvulsant drug. 
# During the first 8-week period, half the patients received a placebo and half were given the drug, and the number of seizures were recorded. 
# Following this, the same patients were given the opposite treatment and the number of seizures were recorded. 
# Assuming that the distribution of the difference between the placebo and drug meets the assumption of normality, 
# perform an appropriate test to determine whether there were differences in the number of epileptic seizures with and without the drug.

# a) Make a boxplot of the data
# b) Test the difference.


# Patient	Placebo	Drug
# 1	37	5
# 2	52	23
# 3	68	40
# 4	4	3
# 5	29	38
# 6	32	19
# 7	19	9
# 8	52	24
# 9	19	17
# 10	12	14

##########################################################################################################################


# Put values into vectors
placebo = c(37, 52, 68, 4, 29, 32, 19, 52, 19, 12)
drug = c(5, 23, 40, 3, 38, 19, 9, 24, 17, 14)

# a) Create boxplot
boxplot(placebo, drug, names=c("Placebo", "Drug"), main="Boxplot of Number of Seizures")

# b) Test the difference
# Paired t-test: Both treatments are applied to every sampled unit.
t.test(placebo, drug, paired=TRUE)

# View p-value:
t.test(placebo, drug, paired=TRUE)$p.value

# The p-value is < 0.05, which indicates a significant difference.
