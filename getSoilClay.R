
rm(list=ls())

library(rgdal)
library(gdalUtils)
library(sf)

url = "https://files.isric.org/soilgrids/latest/data/"

# variable of interest
voi = "clay"
depth = "15-30cm"
quantile = "mean"

variable = paste(url, voi, sep="")
layer = paste(variable,depth,quantile, sep="_")
voi_layer = paste(layer, '.vrt', sep="")

webdav_path = '/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/'

dat_lat <- dat_rs[complete.cases(dat_rs$Latitude),]
dat_coords <- dat_lat[complete.cases(dat_lat$Longitude),]

Tdata = st_as_sf(dat_coords, coords = c("Longitude", "Latitude"),
                 crs = 4326)

igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
Tdata_igh=st_transform(Tdata, igh)

data_igh = data.frame(st_coordinates(Tdata_igh),
                    id = Tdata_igh$Record_number)


fun_pixel_values=function(rowPX,data,VOI,VOI_LYR){
  as.numeric(
    gdallocationinfo(
      srcfile=paste0(webdav_path,"/",VOI,"/", VOI_LYR,".vrt"),
      x=data[rowPX,"X"],
      y=data[rowPX,"Y"],
      geoloc=TRUE,
      valonly=TRUE))
}

gdal_setInstallation(ignore.full_scan=FALSE)
value_pixels = unlist(lapply(1:3,function(x){fun_pixel_values(x,data_igh,voi,voi_layer)}))

