# Problem Set: Fundamentals of Statistics
# Problem Number 6
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Imagine height is genetically determined by the combined (that is, the sum) effect of several genes (polygenic trait). 
# Assume that each gene has an effect on height as a uniform distribution with min=1 and max=3. 
# Simulate an stochastic model of height for 1,000 random people based on 1 gene, 2 genes, and 5 genes. 
# As we increase the number of genes, what is the resulting height distribution?

##########################################################################################################################


hist(runif(1000,1,3))

hist((runif(1000,1,3)) + runif(1000,1,3))

hist(runif(1000,1,3) + runif(1000,1,3) + runif(1000,1,3) + runif(1000,1,3) + runif(1000,1,3))


# As we increase the number of genes, what is the resulting height distribution?
# As we increase the number of genes, the height distribution becomes more narrow and less variable.
# Increasing sample size reduces sampling error / spread. 