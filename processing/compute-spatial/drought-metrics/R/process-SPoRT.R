#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))
UMRB_sf = read_sf(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

time = Sys.time()

#if there likely isnt a new image
if(hour(time) < 12){
  date = Sys.Date() - 1
} else {
  date = Sys.Date()
}
  
url = paste0(
  'https://nssrgeo.ndc.nasa.gov/SPoRT/modeling/lis/conus3km/geotiff/vsm_percentiles/',
  str_replace_all(date, "[[:punct:]]", ""),
  '_0000_sport_lis_vsm0-100cm_percentile_conus3km_float_wgs84.tif')

data = terra::rast(url) %>%
  crop(., UMRB_sf) %>%
  mask(., UMRB_sf) %>%
  terra::subst(., 9999, NA) %>%
  setNames('soil_moisture')%>%
  terra::app(., function(x){qnorm(x/100)})

#write out data
#prevent raster from writing .xml auxilary files
rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")

writeRaster(data, '/home/zhoylman/mco-drought-indicators-data/SPoRT/soil-moisture/sport_soil_moisture_anom.tif', overwrite=TRUE)

rescaled = data * 10

writeRaster(rescaled, '/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/sport_soil_moisture_anom.tif', datatype='INT2S',  overwrite=TRUE, NAflag = -32767)

#write simple txt
fileConn<-file("/home/zhoylman/mco-drought-indicators-data/SPoRT/soil-moisture/time.txt")
writeLines(date %>% as.character(), fileConn)
close(fileConn)
