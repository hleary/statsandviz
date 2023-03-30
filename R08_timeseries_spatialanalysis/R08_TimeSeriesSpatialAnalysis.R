#####################################################################################
## 08. Time Series
#####################################################################################
data(AirPassengers)
AP<-AirPassengers
class(AP)
start(AP)
end(AP)
frequency(AP)

plot(AP, ylab="Passengers (1000's)")

layout(1:2)
plot(aggregate(AP)) #we remove seasonal effect by aggregating to the annual level
boxplot(AP ~ cycle(AP))
layout(1:1)

#Moving average smoothing
library(TTR)
AP.ma<-SMA(AP, n=5)

plot(AP, type="l", col="black", ylab="Passengers (1000's)")
lines(AP.ma, col="red", lwd=2)

#Decomposition
AP.decomp<-decompose(AP)
plot(AP.decomp)

#Autocorrelation function
acf(AP)
#Let's remove the trend and seasonal variation
acf(AP.decomp$random[!is.na(AP.decomp$random)]) #seasonal adjustment has not been entirely effective

#Forecasting (ARIMA models)
library(forecast)
AP.fit<-auto.arima(AP) #identifies the best model based on information criteria
summary(AP.fit)
checkresiduals(AP.fit) #residuals should look like white noise
AP.fcast<-forecast(AP.fit)
plot(AP.fcast)

#####################################################################################
## 08. Spatial Analysis
#####################################################################################
#Mapping
library(maps)
library(ggplot2)
library(viridis)

world_map <- map_data("world")
ggplot(world_map)+
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray", colour = "black", size = 0.1)+
  theme_bw()

eu_map<-map_data("world", region=c("Spain", "Portugal", "France", "Italy"))
ggplot(eu_map, aes(x=long, y=lat))+
  geom_polygon(aes(group=group, fill=region))+
  theme_void()

japan_map<-map_data("world", region="japan")
ggplot(japan_map)+
  geom_polygon(aes(x = long, y = lat, group=group, fill=subregion))+
  theme_bw()

cities<-get('world.cities')
head(cities)
cities_japan<-cities[cities$country.etc == 'Japan',]

ggplot(japan_map)+
  geom_polygon(aes(x = long, y = lat, group=group, fill=subregion))+
  geom_point(data=cities_japan, aes(x=long, y=lat), size=0.1)+
  theme_bw()

ggplot(japan_map)+
  geom_polygon(aes(x = long, y = lat, group=group), fill='lightgray', color="black", size=0.1)+
  geom_point(data=cities_japan, aes(x=long, y=lat, size=pop))+
  theme_bw()

ggplot(japan_map)+
  geom_polygon(aes(x = long, y = lat, group=group), fill='lightgray', color="black", size=0.1)+
  geom_point(data=cities_japan, aes(x=long, y=lat, size=pop), alpha=0.8)+
  theme_bw()

ggplot(japan_map)+
  geom_polygon(aes(x = long, y = lat, group=group), fill='lightgray', color="black", size=0.1)+
  geom_point(data=cities_japan, aes(x=long, y=lat, size=pop, color=pop), alpha=0.8)+
  scale_size_continuous(range=c(1,12))+
  scale_color_viridis(trans="log")+
  theme_void()

arrests<-USArrests
arrests$region<-tolower(rownames(USArrests))

usa_map<-map_data("state")
library(dplyr)
arrests_map<-left_join(usa_map, arrests, by="region") #this is called a cloropleth map

ggplot(arrests_map, aes(long, lat, group=group))+
  geom_polygon(aes(fill=Assault), color="white")+
  theme_bw()

ggplot(arrests_map, aes(long, lat, group=group))+
  geom_polygon(aes(fill=Assault), color="white")+
  scale_fill_viridis(option="C")+
  theme_bw()

#Interactive maps (https://rstudio.github.io/leaflet/)
library(leaflet)

m <- leaflet()
m <- addTiles(m)
m <- addCircleMarkers(m, lng=-110.954161, lat=32.231143)
m

#Download WorldClim data (https://www.worldclim.org/)
#other datasets: SRTM for elevation, GADM for administrative boundaries
library(raster)
library(sp)

climate<-getData('worldclim', var="bio", res=10) #http://worldclim.org/bioclim

plot(climate$bio1, main="Mean Annual Temperature")
plot(climate$bio12, main="Annual Precipitation")

#Download GBIF data (https://www.gbif.org/)
library(rgbif)

occ_count(georeferenced = T)
lynx_pardinus_gbif<-occ_search(scientificName = "Lynx pardinus", hasCoordinate = T)
lynx_rufus_gbif<-occ_search(scientificName = "Lynx rufus", hasCoordinate = T)

lynx_pardinus<-as.data.frame(lynx_pardinus_gbif$data[,c("decimalLatitude", "decimalLongitude")])
lynx_rufus<-as.data.frame(lynx_rufus_gbif$data[,c("decimalLatitude", "decimalLongitude")])

library(maps)
library(ggplot2)
world=map_data("world")

ggplot(world, aes(long, lat))+
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray", colour = "black", size = 0.1)+
  geom_point(data=lynx_pardinus, aes(x=decimalLongitude, y=decimalLatitude), color="red", alpha=0.4)+
  geom_point(data=lynx_rufus, aes(x=decimalLongitude, y=decimalLatitude), color="blue", alpha=0.4)

iberian_map<-map_data("world", region=c("Spain", "Portugal"))

ggplot(iberian_map)+
  geom_polygon(aes(x = long, y = lat, group=group), fill="lightgray")+
  geom_point(data=lynx_pardinus, aes(x=decimalLongitude, y=decimalLatitude), color="red", alpha=0.4, size=3)+
  theme_bw()

#Point pattern analysis
library(spatstat) #package for point pattern analysis

data(cells)
summary(cells) #ppp object
plot(cells)

#Generate a random point pattern using the Poisson process
plot(rpoispp(100))

#Ripley's K
cells.K<-Kest(cells, correction = "none")
plot(cells.K) #line below red indicates dispersion

#Spatial autocorrelation (Moran's I)
library(spatial)
data(topo, package="MASS")

ggplot(topo, aes(x=x, y=y))+
  geom_point(aes(size=z))

topo.dists <- as.matrix(dist(cbind(topo$x, topo$y))) #create matrix of distances
topo.dists.inv <- 1/topo.dists
diag(topo.dists.inv) <- 0 #specify that the diagonal are zeros...just how it's done...

library(ape)
Moran.I(topo$z, topo.dists.inv) #strong spatial autocorrelation

#Spatial regression
sp.data<-read.table(file = "spatialdata.txt", header=T)

ggplot(sp.data, aes(x=longitude, y=latitude))+
  geom_jitter(aes(size=yield), alpha=0.6)

ggplot(sp.data, aes(x=latitude, y=yield))+
  geom_point(alpha=0.6, size=3) # see a general trend

ggplot(sp.data, aes(x=longitude, y=yield))+
  geom_point(alpha=0.6, size=3) # see a general trend

anova(lm(yield~variety, data=sp.data)) #Not significant # this model without any spatial variable
sp.lm<-lm(yield~variety, data=sp.data)
sp.data$resid <-resid(sp.lm)

ggplot(sp.data, aes(x=longitude, y=latitude))+
  geom_jitter(aes(size=resid), alpha=0.6) # Plot residuals, seems autocorrelated

resid.dists <- as.matrix(dist(cbind(sp.data$latitude, sp.data$longitude)))
resid.dists.inv <- 1/resid.dists
diag(resid.dists.inv) <- 0

Moran.I(sp.data$resid, resid.dists.inv) #strong spatial autocorrelation
# that means that the model we just did is wrong, we need to consider autocorrelation/spatial variable
# Check out package "spaMM" for this, but too advanced for class.
# "INLA" uses bayesian stats to model space/time data