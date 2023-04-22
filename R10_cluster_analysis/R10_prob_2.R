# Problem Set: Cluster Analysis
# Question Number 2
# Status: Completed


# 2. Perform k-means partitions from 2 to 6 on the dune_bio.txt dataset 
# and select the best partition using the Calinski criterion. 
# Visualize the results.

# Read in data
dune_bio <- read.table("R10_cluster_analysis/data/dune_bio.txt", sep="\t", header = T, row.names = 1)
head(dune_bio)

# Load packages
library(vegan)       # cluster analysis
library(tidyverse)   # data manipulation
library(fpc)         # calinski criterion

# Calculate the Bray-Curtis distance matrix
distance_matrix <- vegdist(dune_bio, method = "bray")

# Create an empty list to store k-means models
kmeans_models <- list()
calinski_scores <- c()

# Iterate through k = 2 to 6
for (k in 2:6) {
  # Perform k-means clustering using the distance matrix
  model <- kmeans(distance_matrix, centers = k)
  
  # Store the model in the list
  kmeans_models[[k - 1]] <- model
  
  # Calculate dissimilarity sums of squares between and within clusters
  cluster_diss <- cluster.stats(distance_matrix, model$cluster)
  
  # Calculate the Calinski-Harabasz index
  calinski <- cluster_diss$ch
  calinski_scores <- c(calinski_scores, calinski)
}

# Find the optimal number of clusters with the highest Calinski-Harabasz index
optimal_k <- which.max(calinski_scores) + 1

# Select the best k-means model
best_kmeans_model <- kmeans_models[[optimal_k - 1]]

# Output the optimal number of clusters and the best k-means model
optimal_k
best_kmeans_model



