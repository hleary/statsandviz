# Problem Set: Fundamentals of Statistics
# Problem Number 5
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Demonstrate graphically the central limit theorem (by sampling and calculating the mean) 
# using a Binomial distribution with 10 trials (size=10) 
# and 0.9 probability of success (prob=0.9) 
# and a sample size of 5.

##########################################################################################################################


# Create a vector to store the mean of each sample
means <- numeric(1000)

# Loop to generate 1000 samples of size 5 from the Binomial distribution
for (i in 1:1000) {
  sample <- rbinom(5, size= 10, prob= 0.9) # generate sample of size 5
  means[i] <- mean(sample) # calculate mean of the sample
}

# Plot the histogram of the sample means
hist(means, main = "Histogram of Sample Means", xlab = "Sample Mean")

