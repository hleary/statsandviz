# Problem Set: R Language
# Problem Number 3
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Write a logistic growth function
# that also generates a plot with time on the x axis and Nt on the y axis. 
# Using that function create plots for 20 days 
# assuming an initial population size of 10 individuals and a carrying capacity of 1,000 individuals 
# under three growth rate scenarios (0.5, 0.8, 0.4).

###########################################################################################################################

# Logistic growth function.
Ni <- 10
t <- c(1:20)
K <- 1000

log_growth <- function (r) {
  Nt <- K*Ni/(Ni+(K-Ni) * exp(1)^(-r*t))
  plot(t, Nt, xlab="Time", ylab="Population size")
       }

# Test under three growth rate scenarios
log_growth(0.5)
log_growth(0.8)
log_growth(0.4)


