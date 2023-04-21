#####################################################################################
## 09. Diversity Analysis
#####################################################################################

library(vegan)

#read datasets
bci<-read.table("BCI.txt", sep="\t", header=T, row.names=1)
vare.bio<-read.table("vare_bio.txt", sep="\t", header=T, row.names=1)
vare.env<-read.table("vare_env.txt", sep="\t", header=T, row.names=1)

#Species abundance distribution
prestonfit(as.numeric(bci[5,]))
plot(prestonfit(as.numeric(bci[5,])))

#Rank-abundance curves
plot(rad.lognormal(bci[5,]), lty=2, lwd=2)
rad.lognormal(bci[5,])
plot(radfit(bci[5,]))
radfit(bci[5,])

#Richness
specnumber(vare.bio)

#Evenness
diversity(vare.bio, index="shannon")
diversity(vare.bio, index="simpson")
plot(diversity(vare.bio, index="shannon"), diversity(vare.bio, index="simpson"))
cor.test(diversity(vare.bio, index="shannon"), diversity(vare.bio, index="simpson"))

#Sample-based rarefaction
bci.sample.rare<-specaccum(bci, method="random")
plot(bci.sample.rare, col="blue", ci.type="line", ci.lty=2, lwd=2, xlab="Number of samples", ylab="Number of species")

#Individual-based rarefaction
rarecurve(bci[5,], xlab="Number of individuals", ylab="Number of species")
rarecurve(bci, xlab="Number of individuals", ylab="Number of species")

#Euclidean distances (for environmental data)
vare.env.sta<-decostand(vare.env, method="standardize")
vare.env.euc<-vegdist(vare.env.sta, method="euclidean")

#Jaccard distances (for presence-absence species data)
vare.bio.jac<-vegdist(vare.bio, method="jaccard", binary=T)

#Bray-Curtis distances (for abundance species data)
vare.bio.bray<-vegdist(vare.bio, method="bray")

#Mantel test (correlation between distance matrices)
mantel(vare.env.euc, vare.bio.bray)
mantel(vare.env.euc, vare.bio.jac)

plot(vare.env.euc, vare.bio.bray)
abline(lm(vare.bio.bray~vare.env.euc), col="red")
