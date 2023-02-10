# Problem Set: Fundamentals of Statistics
# Problem Number 7
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Do the same as in the previous problem but now assuming that the combined effect of the genes is multiplicative (not the sum). 
# As we increase the number of genes, what is the resulting height distribution?

##########################################################################################################################

hist(runif(1000,1,3))

hist((runif(1000,1,3)) * runif(1000,1,3))

hist(runif(1000,1,3) * runif(1000,1,3) * runif(1000,1,3) * runif(1000,1,3) * runif(1000,1,3))

# As we increase the number of genes, what is the resulting height distribution?
# As we increase the number of genes, the resulting height distribution becomes more right-skewed. 

