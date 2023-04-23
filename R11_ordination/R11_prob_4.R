# Problem Set: Ordination
# Question Number 4
# Status: Completed


# 4. Perform a constrained correspondence analysis (CCA) 
# on the dune_bio.txt dataset using the first (A1), second (Moisture) 
# and fifth (Manure) variables of dune_env.txt as explanatory.

# Load packages
library(vegan)

# Read in data
dune_bio <- read.table("R11_ordination/data/dune_bio.txt", sep="\t", header = T, row.names = 1)

# Perform CCA
dune_cca <- cca(dune_bio ~ A1 + Moisture + Manure, data = dune_env)

# a) Variation explained by first two constrained axes
dune_cca$CCA

# b) Adjusted R2 of the model
RsquareAdj(dune_cca)$adj.r.squared

# c) Permutation test for overall statistical significance
anova(dune_cca)

# d) Permutation test for marginal statistical significance of explanatory variables
anova(dune_cca, by="margin")

# e) Triplot of ordination results focusing on sites
plot(dune_cca, display = "sites")






