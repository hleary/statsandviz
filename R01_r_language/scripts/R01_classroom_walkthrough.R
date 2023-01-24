# Classroom Walkthrough


#### Plotting a correlation ####

# Read data
data <- t(read.table("r_language/data/data_table.txt", sep = "\t", header = TRUE, row.names = 1))
# t means to transpose (swap rows and columns)
# sep = "\t" tells R to separate by tabs. R is smart but specify this just to be safe.
# row.names = 1 indicates that the first row is not data

# Check class
class(data)

# Tranform to data frame
data <- data.frame(data)

# Perform a correlation
cor.test(data[,1], data[,3])

# Plot the correlation
plot(data[,1], data[,3])







