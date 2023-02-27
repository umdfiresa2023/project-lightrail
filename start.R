library("terra")

jan2000_01<-rast("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200001-geotiff/20000101.tif")

trans <- project(jan2000_01,  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

jan_df<-as.data.frame(trans, xy=TRUE)
