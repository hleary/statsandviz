# Problem Set: Time Series and Spatial Analysis
# Question Number 3
# Status: Completed


# Load packages
library(maps)
library(ggplot2)
library(viridis)

# 3. Download the map of Spain.
spain_map<-map_data("world", region="spain")

# a) Make a basic map of Spain
ggplot(spain_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = subregion)) +
  theme_bw()

# b) Get information on the population of Spanish cities 
# and make a map of Spain including the locations and population of Spanish cities.
cities<-get('world.cities')
head(cities)
cities_spain<-cities[cities$country.etc == 'Spain',]

ggplot(spain_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = subregion)) +
  geom_point(data = cities_spain, aes(x = long, y = lat), size = 0.1) +
  theme_bw()

ggplot(spain_map) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = 'lightgray', color = "black", size = 0.1) +
  geom_point(data = cities_spain, aes(x = long, y = lat, size = pop, color = pop), alpha = 0.8) +
  scale_size_continuous(range = c(1, 12)) +
  scale_color_viridis(trans = "log") +
  theme_void()
