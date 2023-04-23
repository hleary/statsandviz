# Problem Set: Ordination
# Question Number 2
# Status: Completed


# Read in data
dune_bio <- read.table("R10_cluster_analysis/data/dune_bio.txt", sep="\t", header = T, row.names = 1)

# Load vegan library
library(vegan)

# Perform CA
dune.ca <- cca(dune_bio)

# a) How much variation is explained by the two first axes?
# Proportion of variation explained by each axis
summary(dune.ca)

# b) Make a screeplot of the results
screeplot(dune.ca)

# c) Plot the ordination results of the sites
plot(dune.ca, display = "sites")

