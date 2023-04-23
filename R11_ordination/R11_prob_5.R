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
dune_bio <- read.table("R11_ordination/data/dune_bio.txt", sep="\t", header = T, row.names = 1)
dune_env <- read.table("R11_ordination/data/dune_env.txt", sep="\t", header = T)

# Calculate Bray-Curtis distances
dune_bc <- vegdist(dune_bio, method = "bray")

# Perform PERMANOVA with A1, Moisture, and Manure as explanatory variables
dune_perm <- adonis2(dune_bc ~ A1 + Moisture + Manure, data = dune_env, permutations = 9999)
dune_perm

# a) Which explanatory variables are significant?
summary(dune_perm)  # all are significant

# b) What is the explanatory power (R2) of the significant variables?
dune_perm$R2

# c) Perform a new PERMANOVA analysis with 9,999 permutations 
# with A1 as the explanatory variable but constrain the permutations 
# within the Use variable.
dune_perm2 <- adonis2(dune_bc ~ A1, data = dune_env, permutations = 9999, strata = dune_env$Use)
dune_perm2

# d) Plot a NMDS ordination to visually confirm your results in c).
# ggplot2 extension to plot ordination:
# https://github.com/gavinsimpson/ggvegan
# install.packages("remotes")
# remotes::install_github("gavinsimpson/ggvegan")
library(ggvegan)
autoplot(dune_perm2)

# That didn't work so: 

# d) Plot a NMDS ordination to visually confirm your results in c).
plot(dune_nmds, display = "sites")
plot(dune_nmds, dune_env$A1, display = "sites", add = TRUE, pch = 16, col = "red")
title(paste("NMDS ordination with environmental variable (A1)\nPERMANOVA R2 = ", round(dune_perm2$R2, 2), "\n p = ", dune_perm2$`Pr(>F)`, sep = ""))

# Nope...
