library(httr)
library(dplyr)
library(readr)
library(raster)
source('/home/zhoylman/drought_indicators/tribal/R/aggregate_tribal.R') #!!!


watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/watersheds_umrb.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/county_umrb.shp")

timescales = c(7,15,30,60,90)

urls = read_csv('/home/zhoylman/drought_indicators/ndvi/data/urls/url_list.csv', col_names = F) %>%
  t()

tictoc::tic()

for(i in 1:length(timescales)){
  GET(urls[i],
      write_disk('/home/zhoylman/drought_indicators/ndvi/data/maps/temp/temp_zip.zip', overwrite = TRUE))
  
  data = unzip('/home/zhoylman/drought_indicators/ndvi/data/maps/temp/temp_zip.zip', exdir = '/home/zhoylman/drought_indicators/ndvi/data/maps/temp/') %>%
    raster::raster()
  
  time = names(data) %>%
    substr(.,10,19) %>%
    as.Date(., format = "%Y.%m.%d")
  
  writeRaster(data, paste0('/home/zhoylman/drought_indicators/ndvi/data/maps/ndvi_anom_', timescales[i], '.tif'), overwrite = T)
  
  # Extract raster values for each HUC 
  r.vals <- extract(data, watersheds)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create shp file for export and add metadata about last timestamp used
  watersheds_export = watersheds
  watersheds_export$current_time = time
  
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
  
  r.median = nullToNA(r.median)
  #assign watershed aggregate values to shps, define path to export and export
  watersheds_export$average = as.vector(unlist(r.median))
  
  path_file_watershed = paste("/home/zhoylman/drought_indicators/ndvi/data/shp/current_ndvi/", sep = "")
  layer_name = paste("ndvi_anom_watershed_",as.character(timescales[i]), sep = "")
  
  rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  
  #extract raster values for each county
  r.vals <- extract(data, county)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create shp file for export
  county_export = county
  
  r.median = nullToNA(r.median)
  
  #assign county aggregate values to shps, define path to export and export
  county_export$average = as.vector(unlist(r.median))
  path_file_county = paste("/home/zhoylman/drought_indicators/ndvi/data/shp/current_ndvi/", sep = "")
  layer_name = paste("ndvi_anom_county_",as.character(timescales[i]), sep = "")
  
  rgdal::writeOGR(obj=county_export, dsn=path_file_county, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  tribal = aggregate_tribal(data)
  
  path_file_tribe = paste("/home/zhoylman/drought_indicators/ndvi/data/shp/current_ndvi/ndvi_anom_tribal_",as.character(timescales[i]),'.shp', sep = "")
  
  sf::st_write(tribal, path_file_tribe,delete_layer=TRUE)
  
  #remove old zip
  system('rm /home/zhoylman/drought_indicators/ndvi/data/maps/temp/*')
}


tictoc::toc()
