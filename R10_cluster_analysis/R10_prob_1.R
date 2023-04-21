# Problem Set: Cluster Analysis
# Question Number 1
# Status: Completed


# 1. Make dendrograms of the results of hierarchical clustering using 
# the single, average and complete methods. 

# Use the dune_bio.txt dataset after creating a Bray-Curtis distance matrix.


dune_bio <- read.table("R10_cluster_analysis/data/dune_bio.txt", sep="\t", header = T, row.names = 1)
head(dune_bio)

#Hierarchical clustering
dune_bray<-vegdist(dune_bio, method="bray")

clust_single<-hclust(dune_bray, method="single")
clust_avg<-hclust(dune_bray, method="average")
clust_complete<-hclust(dune_bray, method="complete")

plot(clust_single) #dendrogram (tree) visualization
plot(clust_avg)
plot(clust_complete)
