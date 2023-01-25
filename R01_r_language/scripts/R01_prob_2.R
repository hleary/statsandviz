# Problem Set: R Language
# Problem Number 2
# Status: In Progress
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# See READ ME for exponential growth equation. 
# Nt is the population size at time t, N0 is the initial population size, and r is the rate of growth or reproductive rate.
# Write an exponential growth function that also generates a plot with time on the x axis and Nt on the y axis.
# Using that function, create plots for 20 days assuming an initial population size of 10 individuals under
# three growth rate scenarios (0.5, 0.8, -0.1).

###########################################################################################################################


# Load libraries
library("tidyverse")


# Write an exponential growth function.
Ni <- 10
t <- c(1:20)

Nt <- function(r){
  Nt <-Ni * exp(r*t)
}

# Plot
plot(t, Nt(0.5))
plot(t, Nt(0.8))
plot(t, Nt(-0.1))




