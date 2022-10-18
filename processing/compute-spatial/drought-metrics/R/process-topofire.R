#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

topofire = raster('/home/zhoylman/topofire_temp/soil_moisture/soil_moisture_anomoly_current.tif')

template = raster('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/current_spi_year_to_date.tif')

clipped = crop(topofire, UMRB) %>%
  mask(., UMRB) %>%
  resample(., template)

#prevent raster from writing .xml auxilary files
rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")

writeRaster(clipped, '/home/zhoylman/mco-drought-indicators-data/topofire/soil-moisture/topofire_soil_moisture_anom.tif', overwrite=TRUE)

rescaled = clipped * 10

writeRaster(rescaled, '/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/topofire_soil_moisture_anom.tif', datatype='INT2S',  overwrite=TRUE, NAflag = -32767)

system('cp /home/zhoylman/topofire_temp/soil_moisture/time.txt /home/zhoylman/mco-drought-indicators-data/topofire/soil-moisture/time.txt')
