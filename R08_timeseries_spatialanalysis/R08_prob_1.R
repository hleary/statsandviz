# Problem Set: Time Series and Spatial Analysis
# Question Number 1
# Status: Completed

# Write a function to calculate the 3-point moving average for an input vector. 
# The formula is:
#   y_i^'=  (y_(i-1)+y_i+y_(i+1))/3

moving_average <- function(x) {
  n <- length(x)
  y <- numeric(n) # Initialize an empty vector to store the moving average values
  
  for (i in 2:(n - 1)) {
    y[i] <- (x[i - 1] + x[i] + x[i + 1]) / 3
  }
  
  return(y)
}

# Example:
input_vector <- c(1, 2, 3, 4, 5, 6, 7)
result <- moving_average(input_vector)
print(result)
