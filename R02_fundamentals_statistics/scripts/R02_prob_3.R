# Problem Set: Fundamentals of Statistics
# Problem Number 3
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# The data in the file DesertBirdAbundance.csv are from a survey of the breeding birds of Organ Pipe Cactus National Monument in southern Arizona.
#   a) Draw a histogram of the abundance data.
#   b) Calculate the median and the mean of the bird abundance data.
#   c) In this particular case, which do you think is the best measure of center, the mean or the median?
#   d) Calculate the range, standard deviation, variance and coefficient of variation of the bird abundance data.

##########################################################################################################################


# Load libraries
# library(tidyverse)
# library(ggplot2)

# Read in the Desert Bird Abundance csv file.
birdset <- read.csv(file = "C:\\Users\\hlear\\Desktop\\statsandviz\\R02_fundamentals_statistics\\data\\DesertBirdAbundance.csv", header = TRUE)

# Explore
head(birdset)
class(birdset)


# a) Histogram using ggplot2
ggplot(birdset, aes(x=abundance))+   
  geom_histogram(fill = "steelblue") +
  labs(title = "Histogram of Desert Bird Abundance", x = "Abundance", y = "Count")

# b) Median and Mean
mean(birdset$abundance)
median(birdset$abundance)

# c) Best measure of center in this case? 
# The median

# d) Range, SD, variance, and CV
range(birdset$abundance)
sd(birdset$abundance)
var(birdset$abundance)

coef_var <- function(x) {
  sd(x) / mean(x) * 100
}
coef_var(birdset$abundance)
  
  