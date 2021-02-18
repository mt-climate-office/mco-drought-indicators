########### HAS NOT BEEN MODIFIED YET!!!

library(raster)
library(dplyr)

source("/home/zhoylman/drought_indicators/zoran/R/fdates.R")

write.dir = "/home/zhoylman/drought_indicators/sedi_app/maps/current_sedi/"
write.dir.shp = "/home/zhoylman/drought_indicators/sedi_app/shp"
read.dir = "/home/zhoylman/drought_indicators/sedi_app/maps/topofire/current"
time.dir = "/home/zhoylman/drought_indicators/sedi_app/maps/topofire/current_time/time"

current_time = read.table(time.dir, sep = "\t") %>%
  sapply(., as.character) %>%
  fdates() %>%
  as.Date(., format = "%Y%m%d") %>%
  max(., na.rm = T)

#import shps
UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/outline_umrb.shp")
watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/watersheds_umrb.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/county_umrb.shp")

#define some functions for clipping
crop_to_umrb = function(x){
  crop(x, extent(UMRB)) %>%
    mask(., UMRB)
}

extract_shp_data = function(map, shp){
  # Extract raster values for each shp 
  shp.vals = extract(map, shp)
  # Use list apply to calculate median for each polygon
  r.median = lapply(shp.vals, FUN=median, na.rm=TRUE) %>%
    nullToNA()
  return(as.vector(unlist(r.median)))
}

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#process maps
maps = list.files(read.dir, full.names = T) %>%
  lapply(.,raster)%>%
  lapply(., crop_to_umrb) 

raster_names = paste0(write.dir, list.files(read.dir))

raster_name_times = raster_names %>%
  stringr::str_extract(., "\\d+")
  
path_file_watershed = paste("/home/zhoylman/drought_indicators/sedi_app/shp/current_sedi/", sep = "")
path_file_county = paste("/home/zhoylman/drought_indicators/sedi_app/shp/current_sedi/", sep = "")

# write out all data
for(i in 1:length(maps)){
  writeRaster(maps[[i]], raster_names[[i]], overwrite = T)
  
  watersheds_export = watersheds
  watersheds_export$current_time = current_time
  watersheds_export$average = extract_shp_data(maps[[i]],watersheds)
  layer_name = paste("current_sedi_watershed_",raster_name_times[i], sep = "")
  
  rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  county_export = county
  county_export$current_time = current_time
  county_export$average = extract_shp_data(maps[[i]],county)
  layer_name = paste("current_sedi_county_",raster_name_times[i], sep = "")
  
  rgdal::writeOGR(obj=county_export, dsn=path_file_county, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
}
