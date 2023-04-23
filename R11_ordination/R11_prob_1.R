# Problem Set: Ordination
# Question Number 1
# Status: Completed


# 1. Load the varechem dataset within the R package vegan. 
# This data frame collects soil characteristics. 

library(vegan)

# Load the varechem dataset
data(varechem)

# View
head(varechem)


# Given the nature of this dataset perform a PCA or a CA ordination analysis.
varechem.pca <- rda(varechem)


# a) How much variation is explained by the two first axes?

# eigenvalues of the PCA
eigenvalues <- varechem.pca$CA$eig

# proportion of variation explained by each axis
summary(varechem.pca)
(prop.var <- round(eigenvalues/sum(eigenvalues)*100, 1))

# b) Make a screeplot of the results
plot(prop.var, type = "b", xlab = "Principal component", ylab = "Proportion of variance", main = "Screeplot of PCA")


# c) Plot the ordination results of the sites
plot(varechem.pca, type = "n")
text(varechem.pca, display = "sites", cex = 0.7)

# d) Plot both the sites scores and the soil characteristics scores focusing on the soil variables
biplot(varechem.pca, scaling = 2)


