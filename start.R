install.packages("raster")

library("raster")
library(tidyverse)

# monthly maps for 2000
jan00<-raster("C:/Users/Samirah/Downloads/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-annual-geotiff/Annual-geotiff/2000.tif")
feb00<-raster("C:/Users/Samirah/Downloads/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200003-geotiff/20000301.tif")
# mar00<-raster("")

  
df<-jan00@data

monthly_avg<-data.frame(Year = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))

monthly_avg<-cbind(monthly_avg, Jan = NA, Feb = NA, Mar = NA, Apr = NA, May = NA, Jun = NA, July = NA, Aug = NA, Sep = NA, Oct = NA, Nov = NA, Dec = NA)

