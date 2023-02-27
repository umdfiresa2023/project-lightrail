install.packages("raster")
install.packages("terra")

library("raster")
library(tidyverse)

# monthly maps for 2000
jan2000_01<-raster("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200001-geotiff/20000101.tif")
jan2000_02<-raster("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200001-geotiff/20000102.tif")
jan2000_03<-raster("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200001-geotiff/20000103.tif")

# trans3<-spTransform(jan2000_01, CRS("+init=epsg:4326"))
trans3 <- projectRaster(jan2000_01,  crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
# trans3<-projectRaster(jan2000_01, crs="+init=epsg:4326")
jan_df<-as.data.frame(jan2000_01, xy=TRUE)


df<-jan00@data

monthly_avg<-data.frame(Year = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))

monthly_avg<-cbind(monthly_avg, Jan = NA, Feb = NA, Mar = NA, Apr = NA, May = NA, Jun = NA, July = NA, Aug = NA, Sep = NA, Oct = NA, Nov = NA, Dec = NA)

