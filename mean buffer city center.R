#install.packages("terra")
#install.packages("rgeos")
#install.packages("tidyverse")
#install.packages("tmap")
#install.packages("leaflet")
install.packages("remotes")
remotes::install_github("rstudio/leaflet")
library("terra")
library("rgeos")
library("tidyverse")
library("tmap")
library("leaflet")
library("maptiles")
library("leafem")

cities_latlon<-read.csv("output/cities_latlon.csv")

jan2000_01<-rast("input/pm25_raster/20000101.tif")

i<-1

city_coord<-cities_latlon[1,2:3]

pts<-vect(city_coord, 
          crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

pts_buffer<-terra::buffer(pts, width = 10000)

pts_extent<-terra::buffer(pts, width = 20000)

plot(pts_buffer)

#work with raster
jan2000_01<-rast("input/pm25_raster/20000101.tif")

raster_project <- project(jan2000_01,  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

int<-crop(raster_project, pts_buffer,
          snap="in",
          mask=TRUE)

#visualize data
plot(int, ext=ext(pts_extent))
lines(pts_buffer)

#turn raster circle into dataframe
v <- as.data.frame(values(int))

names(v)<-"pm25"
  
#jan_df<-as.data.frame(int)
#find pm2.5 average within the circle dataframe
v2<-v %>%
  filter(!is.na(pm25)) %>%
  summarize(mean_pm25 = mean(pm25))

