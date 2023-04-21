# Problem Set: Time Series and Spatial Analysis
# Question Number 4
# Status: Completed

# 4. Download GBIF georeferenced occurrence data of the Pyrenean desman (Galemys pyrenaicus) 
# and make a map of its geographical distribution 
# Hint: it is only present in Spain, Andorra, Portugal and France).


# Load packages
library(rgbif)
library(maps)
library(ggplot2)

# Download from GBIF
desman_gbif<-occ_search(scientificName = "Galemys pyrenaicus", hasCoordinate = T)

# Convert to data frame
desman<-as.data.frame(desman_gbif$data[,c("decimalLatitude", "decimalLongitude")])

# Download maps for geographical distribution
southEU_map<-map_data("world", region=c("Spain", "Portugal", "France", "Andorra"))

# Plot distribution
ggplot(southEU_map)+
  geom_polygon(aes(x = long, y = lat, group=group), fill="lightgray")+
  geom_point(data=desman, aes(x=decimalLongitude, y=decimalLatitude), color="red", alpha=0.4, size=2)+
  theme_bw()
