# Problem Set: Ordination
# Question Number 5
# Status: Not completed


# 5. Perform a permutational multivariate analysis of variance (PERMANOVA) 
# with 9,999 permutations using the dune_bio.txt dataset 
# after calculating Bray-Curtis distances 
# and the first (A1), second (Moisture) and fifth (Manure) variables 
# of dune_env.txt as explanatory.

# Load packages
library(vegan)

# Read in data
dune_bio <- read.table("R11_ordination/data/dune_bio.txt", sep = "\t", header = TRUE, row.names = 1)
dune_env <- read.table("R11_ordination/data/dune_env.txt", sep = "\t", header = TRUE)

# Convert Use variable to a factor
dune_env$Use <- factor(dune_env$Use)

# Calculate Bray-Curtis distances
dune_bc <- vegdist(dune_bio, method = "bray")

# Perform PERMANOVA with A1, Moisture, and Manure as explanatory variables
dune_perm <- adonis2(dune_bc ~ A1 + Moisture + Manure, data = dune_env, permutations = 9999)

# a) Which explanatory variables are significant?
print(summary(dune_perm))

# b) What is the explanatory power (R2) of the significant variables?
print(dune_perm$R2)

# c) Perform a new PERMANOVA analysis with 9,999 permutations 
# with A1 as the explanatory variable but constrain the permutations within the Use variable.
dune_perm2 <- adonis2(dune_bc ~ A1, data = dune_env, permutations = 9999, strata = dune_env$Use)

# Print results of PERMANOVA with constrained permutations
print(summary(dune_perm2))


# d) Plot a NMDS ordination to visually confirm your results in c).
# ggplot2 extension to plot ordination:
# https://github.com/gavinsimpson/ggvegan
# install.packages("remotes")
# remotes::install_github("gavinsimpson/ggvegan")
# library(ggvegan)
# library(ggfortify)
# autoplot(dune_perm2)

# That didn't work, so...


# d) Plot a NMDS ordination to visually confirm your results in c).
# Perform NMDS ordination
dune_nmds <- metaMDS(dune_bio, distance = "bray")

# Add environmental variables to the ordination plot
ordiplot(dune_nmds, display = "sites", type = "n")
points(dune_nmds, display = "sites", col = as.numeric(dune_env$Use), pch = 16)
ordiellipse(dune_nmds, dune_env$A1, kind = "se", conf = 0.95, label = TRUE)

# Add title and legend to the plot
title(main = "NMDS Ordination with A1 as Explanatory Variable")
legend("bottomright", legend = levels(dune_env$Use), col = 1:length(levels(dune_env$Use)), pch = 16, title = "Use")

