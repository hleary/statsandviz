# Problem Set: Diversity Analysis
# Question Number 1
# Status: Completed


# 1. Load the dune_bio.txt dataset. 
# Species should be in columns and sites in rows.
dune <-read.table("R09_diversity_analysis/data/dune_bio.txt", header=T)
head(dune)

# Load packages
library(vegan)

# a) Calculate the total number of individuals of all species
sum(dune)

# b) Calculate the total number of individuals for each species
colSums(dune)

# c) Calculate the average number of individuals for each species
colMeans(dune)

# d) Calculate the total number of individuals for each site
rowSums(dune)

# e) Calculate the average number of individuals for each site
rowMeans(dune)

# f) Function to report the median number of individuals for each species and each site
median_sps_sites<-function(x){
  cat("The median number of inds per sp is", apply(x, 2, median), "and for sites is", apply(x, 1, median))
}

# Median number of individuals for each species
median_sps_sites(dune)

# g) Transform the dataset to relative abundances using decostand()
dune_relabun <-decostand(dune, method = "total")
rowSums(dune_relabun)

# h) Standardize the dataset into the range 0 to 1 using decostand()
dune_range<-decostand(dune, method = "range")

# i) Standardize the dataset to mean=0 and variance=1 using decostand()
dune_stand<-decostand(dune, method = "standardize")

