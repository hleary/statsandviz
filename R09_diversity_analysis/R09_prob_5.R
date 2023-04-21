# Problem Set: Diversity Analysis
# Question Number 5
# Status: Completed


# 5. Make a rank-abundance curve using the second site (13) 
# of the dune_bio.txt dataset. Fit a lognormal model to the data.

dune <- read.table("R09_diversity_analysis/data/dune_bio.txt", header = T)
head(dune)

#Rank-abundance curves
plot(rad.lognormal(dune[2,]), lty=2, lwd=2)
rad.lognormal(dune[2,])
plot(radfit(dune[2,]))
radfit(dune[2,])
