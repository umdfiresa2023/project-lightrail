install.packages("terra")
install.packages("rgeos")
install.packages("tidyverse")
install.packages("tmap")
install.packages("leaflet")

library("terra")
# library("rgeos")
library("tidyverse")
library("tmap")
library("leaflet")

cities_latlon<-read.csv("output/cities_latlon.csv")

jan2000_01<-rast("input/pm25_raster/20000101.tif")

i<-1

city_coord<-cities_latlon[1,]

#turn city_coord into a point spatial file and create a buffer

city_spat<-vect(city_coord, geom=c("lon", "lat"), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs", keepgeom=FALSE)

#coordinates(city_coord) <- ~lon+lat
#crs(city_coord)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
city_buffer<-terra::buffer(city_spat, width = 1)

#turn raster into wgs84
raster_project <- project(jan2000_01,  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

ext<-ext(raster_project)

#crop raster within circle
intersection<-terra::intersect(ext, city_spat)

tm_shape(city_buffer) + tm_borders()

m<-plot(city_buffer)
m

#turn raster circle into dataframe
jan_df<-as.data.frame(intersection)

#find pm2.5 average within the circle dataframe
mean_jan<-jan_df %>%
  summarize(mean_pm25 = mean(PM25))

