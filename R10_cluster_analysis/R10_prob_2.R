# Problem Set: Cluster Analysis
# Question Number 2
# Status: not completed


# 2. Perform k-means partitions from 2 to 6 on the dune_bio.txt dataset 
# and select the best partition using the Calinski criterion. 
# Visualize the results.

dune_bio <- read.table("R10_cluster_analysis/data/dune_bio.txt", sep="\t", header = T, row.names = 1)
head(dune_bio)



#Non-hierarchical clustering (K-means)
dune_bio_k<-cascadeKM(dune_bio, inf.gr=2, sup.gr=6)
plot(dune_bio_k, sortg=T)

dune_bio_k$results
dune_bio_k$partition
dune_bio_k$partition[,2]


