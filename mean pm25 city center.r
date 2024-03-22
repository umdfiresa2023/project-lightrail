library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tidyverse)
library("tmap")
library("rgeos")
library("spdplyr")
library(sf)
library("ggmap")
library(googleAuthR)
library(mapsapi)
api <- "" # Text file with the API key
register_google(key = api, write = TRUE)
has_google_key()
google_key()
ggmap_show_api_key()
getOption("ggmap")

citynames <- c("Little Rock Arizona",
               "Denton-Lewisville Texas",
               "Portland Maine",
               "Nashville-Davidson Tennessee",
               "Albuquerque New Mexico",
               "Virginia Beach Virginia", 
               "Charlotte North Carolina",
               "Austin Texas",
               "Phoenix Arizona",
               "Minneapolis-St. Paul Minnesota",
               "Houston Texas") 
for (i in 1:length(citynames)){
  locations <- citynames[i] %>% 
    geocode()
  df <- nc_open('V4NA03_PM25_NA_200401_200401-RH35.nc')
  lon<-df$dim$LON$vals
  lat<-df$dim$LAT$vals
  PM25<-ncvar_get(df, varid="PM25")
  coord<-as.data.frame(crossing(lon,lat))
  coord2 <- coord %>%
    filter(lat >= ((locations$lat)-1) & lat <= ((locations$lat)+1)) %>% # filter coordinates to a box around city center
    filter(lon <= ((locations$lon)+1) & lon >=((locations$lon)-1))
  coordinates(coord2) <- ~lon+lat
  #crs(coord2)<-"+proj=longlat +ellps=WGS84+datum=WGS84 +units=m +no_defs"
  coordinates(locations) <- ~lon+lat
  crs(locations)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  #Step 3 
  #Make a polygon shapefile with rep 10 km buffer around city center
  h2<-spTransform(locations, CRS("+init=epsg:3857"))
  
  
  test<-gBuffer(h2, width = 10000)
  test2<-gBuffer(locations, width = 0.1)
  
  intersect<-gIntersection(coord2, test2)
  wkt <- sf::st_crs(4326)[[2]]
  crs(intersect)<-wkt
  PM25_raster<-raster(t(PM25), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  intersect2<-mask(PM25_raster, intersect)
  intersect3<-intersect(intersect2, intersect)
  pm25_houston<-as.data.frame(intersect3, xy=TRUE)
  
  avgpm25 <- pm25_houston %>%
    summarize(meanpm25 = mean(layer))
  
  avgpm25_2<-pm25_houston %>% 
    select(-layer)
  
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_vec <- as.vector(PM25)
  tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
  names(tmp_df01) <- c("lon","lat","PM25")
  radius_points<-as.data.frame(intersect)
  
  df2<- tmp_df01 %>%
    filter(lon %in% radius_points$x & lat %in% radius_points$y)
  
  df3<- df2 %>%
    summarize(meanpm25 = mean(PM25))
  
  files<-list.files(pattern=".nc")
  
  output<-c()
  
  for (j in 1:length(files)){
    print(j)
    df <- nc_open(files[j])
    PM25<-ncvar_get(df, varid="PM25")
    tmp_vec <- as.vector(PM25)
    length(tmp_vec)
    tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
    names(tmp_df01) <- c("lon","lat","PM25")
    
    df2<- tmp_df01 %>%
      filter(lon %in% radius_points$x & lat %in% radius_points$y)
    
    df3<- df2 %>%
      summarize(meanpm25 = mean(PM25)) %>%
      mutate(files=files[j])
    
    output<-rbind(output,df3)
  }
  #setwd("G:/Shared drives/2022 FIRE-SA/SUMMER INTERNSHIP/Light Rail/OUTPUT")
  #write.csv(output, sprintf('%s.csv', citynames[i]))
}
  
