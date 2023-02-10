# Problem Set: Statistical Tests
# Problem Number 5
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Perform 10 one-sample t-tests for mu=0 on simulated standard normally distributed data of 25 observations each and get the P-value. 
# Repeat the experiment, but instead simulate samples of 25 observations from a t-distribution with 2 degrees of freedom. 
# Find a way to automate the first experiment to do it a 1,000 times and applying a false discovery rate correction to the P-values 

##########################################################################################################################

# Perform 10 one-sample t-tests for mu=0 on simulated standard normally distributed data of 25 observations each and get the P-value.
t.test(rnorm(25), mu=0)$p.value
replicate(10, t.test(rnorm(25), mu=0)$p.value) # replicates x10

# Repeat the experiment, but instead simulate samples of 25 observations from a t-distribution with 2 degrees of freedom. 
t.test(rt(25, df=2), mu=0)$p.value
replicate(10, t.test(rt(25, df=2), mu=0)$p.value) # replicates x10

# Automate x1000 and apply FDR correction
experiment.pval.fdr<-p.adjust(replicate(1000, t.test(rnorm(25), mu=0)$p.value), method="fdr")

# Test
head(experiment.pval.fdr)
length(experiment.pval.fdr) # The number of elements in a vector
