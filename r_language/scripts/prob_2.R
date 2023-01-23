# Problem Set: R Language
# Problem Number 2
# Status: In Progress
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# See READ ME for exponential growth equation. 
# Nt is the population size at time t, N0 is the initial population size, and r is the rate of growth or reproductive rate.
# Write an exponential growth function in R that also generates a plot with time on the x axis and Nt on the y axis.
# Using that function, create plots for 20 days assuming an initial population size of 10 individuals under
# three growth rate scenarios (0.5, 0.8, -0.1).

###########################################################################################################################


# Write an exponential growth function:
# Use this code if N0 = 10 individuals and t = 20 days. 
Nt_func <- function(r){
  10 * exp(r*20)
}

print(Nt_func(0.5)) # Population size under the 0.5 growth rate scenario.


# Use this code to define N0 (shown as Ni for "initial" here) and t.
Nt_func <- function(Ni, r, t){
  Ni * exp(r*t)
}



# Test
print(Nt_func(10,0.5,20)) # Scenario growth rate 0.5
print(Nt_func(10,0.8,20)) # Scenario growth rate 0.8
print(Nt_func(10,-0.1,20)) # Scenario growth rate -0.1


# The function works but must generate a plot as well.
# Load libraries for data viz
library("tidyverse")


# The function works but it must generate a plot with time on the x axis and Nt on the y axis.
# STUCK STUCK STUCK





