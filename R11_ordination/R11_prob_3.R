# Problem Set: Ordination
# Question Number 3
# Status: Completed


# 3. Perform a nonmetric multidimensional scaling (NMDS) on the 
# dune_bio.txt dataset after calculating Bray-Curtis distances among sites.

# Read in data
dune_bio <- read.table("R11_ordination/data/dune_bio.txt", sep="\t", header = T, row.names = 1)

# Calculate Bray-Curtis dissimilarity matrix
dune_dist <- vegdist(dune_bio, method = "bray")

# Perform NMDS
set.seed(123)
dune_nmds <- metaMDS(dune_dist, k = 2, trymax = 100)

# a) What is the stress?
dune_nmds$stress

# b) Make a Shepard plot of the NMDS results
stressplot(dune_nmds)


# c) Plot the ordination results of the sites
plot(dune_nmds, display = "sites")

# d) [Advanced – ggplot2] Plot the ordination results using ggplot2. 
#    Use the dune_env.txt dataset to make the size of the points 
#    proportional to the site richness (number of species) and 
#    the color to represent the variable Management.

# Load ggvegan package
library(ggvegan)

# Load data
dune_env <- read.table("R11_ordination/data/dune_env.txt", sep="\t", header = T, row.names = 1)

# Extract site scores from NMDS object and name columns
dune_scores <- data.frame(NMDS1 = dune_nmds$points[,1], NMDS2 = dune_nmds$points[,2])

# Add site richness and Management variables
dune_scores$Richness <- rowSums(dune_bio > 0)
dune_scores$Management <- dune_env$Management

# Plot using ggplot2
ggplot(dune_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = Richness, color = Management)) +
  scale_color_manual(values = c("black", "red", "blue", "green")) +
  labs(x = "NMDS1", y = "NMDS2", size = "Richness", color = "Management") +
  theme_bw()








