# Problem Set: R Language
# Problem Number 6
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES ##########################################################################################################

# Install the R package ‘nycflights13’, and load the ‘weather’ data.
# a) Explore the columns names and the top part of the dataset to get a sense of the data
# b) Make a subset of the data from just the first month (1) and then save that subset as ‘weather1’
# c) Using ggplot2, make a beautiful histogram of the variable ‘temp’
# d) Using ggplot2, make a beautiful line plot of ‘temp’ as a function of ‘time_hour’
# e) Using ggplot2, make a beautiful boxplot of ‘temp’ as a function of ‘origin’

###########################################################################################################################


# Install the R package ‘nycflights13’, and load the ‘weather’ data.
install.packages("nycflights13")
data(package = "nycflights13") # Run to see data sets within the nycflights13 package. Weather is in there.
library(nycflights13)


# a) Explore the column names and top part of the dataset to get a sense of the data.
colnames(weather) # View column names only.
head(weather) # View top part of the data.


# b) Make a subset of the data from just the first month (1) and then save that subset as ‘weather1’.
weather # View the "weather" data set.
weather1 <- subset(weather, weather$month ==1)

# Test
# The subset was too big to see here, so I ended up viewing it outside of R to confirm.
# There is likely a more efficient method using code, but I don't know it.
# weather1
# write.table(weather1, "C:\\Users\\hlear\\Desktop\\statsandviz\\r_language\\data\\weather1.tab", row.names=FALSE)


# c) Using ggplot2, make a beautiful histogram of the variable ‘temp’
library(tidyverse)
ggplot(data = weather, aes(x = temp)) + geom_histogram() # Not beautiful...

# Make pretty
ggplot(data = weather, aes(x = temp)) + geom_histogram(col = "white", 
                                                       fill = "red") +
  labs(title = "Temperatures during NYC Flight Departures (2013)",
       x = "Temperature (F)",
       y = "Count")


# d) Using ggplot2, make a beautiful line plot of ‘temp’ as a function of ‘time_hour’
ggplot(weather, aes(x = time_hour, y = temp)) + geom_line(color = "steelblue") +
  labs(title = "NYC Departure Temperatures Over Time (2013)",
       x = "Time (hrs)",
       y = "Temp (F)")

# e) Using ggplot2, make a beautiful boxplot of ‘temp’ as a function of ‘origin’
ggplot(weather1, aes(x = origin, y = temp)) +
  geom_boxplot() +
  labs(title = "Temperature by Origin (2013)",
       x = "Origin",
       y = "Temperature (F)")

