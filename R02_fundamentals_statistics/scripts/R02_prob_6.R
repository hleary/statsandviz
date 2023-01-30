# Problem Set: Fundamentals of Statistics
# Problem Number 6
# Status: In Progress
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Imagine height is genetically determined by the combined (that is, the sum) effect of several genes (polygenic trait). 
# Assume that each gene has an effect on height as a uniform distribution with min=1 and max=3. 
# Simulate an stochastic model of height for 1,000 random people based on 1 gene, 2 genes, and 5 genes. 
# As we increase the number of genes, what is the resulting height distribution?

##########################################################################################################################

height <- function(n_genes){
  sum(runif(n_genes * 1000, min= 1, max= 3))
}


hist(height(1), main="1 Gene", xlab="Height")
hist(height(2), main="2 Genes", xlab="Height")
hist(height(5), main="5 Genes", xlab="Height")

# As we increase the number of genes, what is the resulting height distribution?
# To be answered...