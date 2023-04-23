#####################################################################################
## 11. Ordination
#####################################################################################

library(vegan)

#read dataset
vare.bio<-read.table("vare_bio.txt", sep="\t", header=T, row.names=1)
vare.env<-read.table("vare_env.txt", sep="\t", header=T, row.names=1)

#Principal Components Analysis (PCA): linear, preserves the Euclidean distance among sites
#Used for environmental data
#Standardization is needed when variables are not expressed in the same magnitudes
vare.env.pca<-rda(vare.env, scale=T) #function from vegan library (prcomp(vare.env, scale=T) for function from base R)
summary(vare.env.pca)
#eigenvalues can be expressed as proportion of variation explained by dividing by the total inertia (variation)
ev<-vare.env.pca$CA$eig #extract eigenvalues
barplot(ev/sum(ev), main="Eigenvalues", col="bisque") #screeplot
screeplot(vare.env.pca) #screeplot

plot(vare.env.pca, display="sites", xlab="PC1 (37%)", ylab="PC2 (23%)")
biplot(vare.env.pca, scaling="species", xlab="PC1 (37%)", ylab="PC2 (23%)") #Focus on variables (species)
biplot(vare.env.pca, scaling="sites", xlab="PC1 (37%)", ylab="PC2 (23%)") #Focus on objects (sites)

#Correspondence Analysis (CA): unimodal, preserves the Chi-squared distance among sites
#Used for community data (frequencies, dimensionally homogeneous, and non-negative)
vare.bio.ca<-cca(vare.bio)
summary(vare.bio.ca)
screeplot(vare.bio.ca)

plot(vare.bio.ca, display="sites", xlab="CA1 (25%)", ylab="CA2 (17%)")
plot(vare.bio.ca, scaling="species", xlab="PC1 (25%)", ylab="PC2 (17%)") #Focus on variables (species)
plot(vare.bio.ca, scaling="sites", xlab="CA1 (25%)", ylab="CA2 (17%)") #Focus on objects (sites)

#Nonmetric Multidimensional scaling (NMDS): ordination from any distance matrix, not an eigenvector-based method
#Represent the set of objects along a predetermined number of axes while preserving the ordering relationships among them
vare.bio.nmds<-metaMDS(vare.bio, distance="bray", k=2, trymax = 100)
vare.bio.nmds$stress
stressplot(vare.bio.nmds) #Shepard plot

plot(vare.bio.nmds, type="t", display="sites")
plot(vare.bio.nmds, type="t", display="species")
plot(vare.bio.nmds, type="p", display=c("sites", "species"))

#Constrained (or Canonical) Correspondence Analysis (CCA): related to CA
vare.cca<-cca(vare.bio ~ Al + P + K + N, data=vare.env)
summary(vare.cca)
#The constrained fraction is the amount of variance of the species matrix explained by the explanatory variables.
#Expressed as a proportion, it is equivalent to an R^2 in multiple regression, however it needs to be adjusted
RsquareAdj(vare.cca)$adj.r.squared

plot(vare.cca, scaling="species", xlab="CCA1 (18%)", ylab="CCA2 (9%)") #Focus on species
plot(vare.cca, scaling="sites", xlab="CCA1 (18%)", ylab="CCA2 (9%)") #Focus on sites

anova(vare.cca)
anova(vare.cca, by="margin")

#ggplot2 extension to plot ordination:
#https://github.com/gavinsimpson/ggvegan
#install.packages("remotes")
#remotes::install_github("gavinsimpson/ggvegan")
library(ggvegan)
autoplot(vare.env.pca)

#Variation partitioning (2 or more sets of explanatory variables)
vare.part<-varpart(vare.bio, vare.env[,c(1,2,3,7)], vare.env[,c(12,13,14)])
vare.part
plot(vare.part)

#PERMANOVA (for quantitative and categorical variables)
adonis(vegdist(vare.bio, method="bray") ~ Al + P + K + N, data=vare.env)
