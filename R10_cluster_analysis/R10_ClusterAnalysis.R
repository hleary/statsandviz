#####################################################################################
## 10. Cluster Analysis
#####################################################################################

library(vegan)

#read dataset
vare.bio<-read.table("vare_bio.txt", sep="\t", header=T, row.names=1)

#Hierarchical clustering
vare.bio.bray<-vegdist(vare.bio, method="bray")
vare.bio.clust<-hclust(vare.bio.bray, method="average") #UPGMA

plot(vare.bio.clust) #dendrogram (tree) visualization
rect.hclust(vare.bio.clust, k=3, border="red")
rect.hclust(vare.bio.clust, k=2, border="blue")

vare.bio.den<-as.dendrogram(vare.bio.clust)
heatmap(as.matrix(vare.bio.bray), Rowv=vare.bio.den, symm=T) #heatmap visualization

#Non-hierarchical clustering (K-means)
vare.bio.k<-cascadeKM(vare.bio, inf.gr=2, sup.gr=6)
plot(vare.bio.k, sortg=T)

vare.bio.k$results
vare.bio.k$partition
vare.bio.k$partition[,2]
