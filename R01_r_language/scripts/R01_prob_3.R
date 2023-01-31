# Problem Set: R Language
# Problem Number 3
# Status: In Progress
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Write a logistic growth function
# that also generates a plot with time on the x axis and Nt on the y axis. 
# Using that function create plots for 20 days 
# assuming an initial population size of 10 individuals and a carrying capacity of 1,000 individuals 
# under three growth rate scenarios (0.5, 0.8, 0.4).

###########################################################################################################################

# Write an exponential growth function.
Ni <- 10
t <- c(1:20)


Nt <- function(r){
  Nt <- Ni * exp(r*t)
  plot(t, Nt)
}

Nt(0.5)
Nt(0.8)
Nt(0.4)

# Logistic growth function (to incorporate K)




