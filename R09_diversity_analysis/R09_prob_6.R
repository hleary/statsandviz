# Problem Set: Diversity Analysis
# Question Number 6
# Status: Completed


# 6. Create a Euclidean distance matrix after standardization 
# using the first (A1), second (Moisture) and fifth (Manure) variables 
# of dune_env.txt. 
 
# Then create a Bray-Curtis distance matrix using dune_bio.txt. 
# Perform a Mantel test on both distance matrices 
# and plot the relationship.

dune_bio<-read.table("R09_diversity_analysis/data/dune_bio.txt", sep="\t", header=T, row.names = 1)
dune_env<-read.table("R09_diversity_analysis/data/dune_env.txt", sep="\t", header=T, row.names = 1)

env_eucDist<-vegdist(decostand(dune_env[,c(1,2,5)], method="standardize"), method="euclidean")

bio_bray<-vegdist(dune_bio, method="bray")

mantel(env_eucDist, bio_bray)

plot(env_eucDist, bio_bray)


