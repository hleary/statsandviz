# Problem Set: R Language
# Problem Number 1
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Install the ISwR R package. 
# Write the built-in dataset thuesen to a tab-separated text file. 
# View it with a text editor. Change the NA to . (period), and 
# read the changed file back into R.

###########################################################################################################################


# Install and load the ISwR R Package.
install.packages("ISwR")
library("ISwR")


# Write the built-in dataset thuesen to a tab-separated text file. 
data(package = "ISwR") # Run to see the data sets within the ISwR package.
thuesen_df <- as.data.frame(thuesen)  # Read the dataset thuesen in the package ISLR
head(thuesen_df) # View the first few rows of the dataset thuesen
write.table(thuesen_df, "C:\\Users\\hlear\\Desktop\\statsandviz\\R01_r_language\\data\\thuesen_tab.txt", row.names=FALSE) # Don't export row names.


# I viewed thuesen_tab in Notepad and changed "NA" to "." (period)


# Read the changed file back into R.
thuesen_tab <- read.table(file = "C:\\Users\\hlear\\Desktop\\statsandviz\\R01_r_language\\data\\thuesen_tab.txt", header = TRUE) # Has a header
head(thuesen_tab) # Just to check it read in correctly
