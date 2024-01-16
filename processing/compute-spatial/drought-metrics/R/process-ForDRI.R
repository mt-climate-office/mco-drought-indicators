#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

UMRB_sf = read_sf(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

all_dates = seq(as.Date("2024/1/1"), as.Date("2029/1/1"), "weeks")

valid_dates = all_dates[all_dates < Sys.Date()]

date_of_interest = valid_dates[length(valid_dates)]

url = paste0('https://fordri.unl.edu/data/ForDRI/ForDRI_',
             str_replace_all(date_of_interest, "[[:punct:]]", ""),
             '.tif')

data = terra::rast(url) %>%
  crop(., UMRB_sf) %>%
  mask(., UMRB_sf) 

forest_cover = terra::rast('/home/zhoylman/mco-drought-indicators/processing/base-data/processed/modis_forest_cover_gte_10.tif') %>%
  terra::project(., data)

forest_cover[forest_cover > 0] = 1
forest_cover[forest_cover == 0] = NA

data_masked = data * forest_cover

#write out data
#prevent raster from writing .xml auxilary files
rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")

writeRaster(data_masked, '/home/zhoylman/mco-drought-indicators-data/ForDRI/current_ForDRI.tif', overwrite=TRUE)

rescaled = data_masked * 10

writeRaster(rescaled, '/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/current_ForDRI.tif', datatype='INT2S',  overwrite=TRUE, NAflag = -32767)

#write simple txt
fileConn<-file("/home/zhoylman/mco-drought-indicators-data/ForDRI/time.txt")
writeLines(date_of_interest %>% as.character(), fileConn)
close(fileConn)
