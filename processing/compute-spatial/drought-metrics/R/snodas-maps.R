#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'
snodas.dir = paste0(export.dir, 'snodas/')

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))
source(paste0(git.dir,"/processing/ancillary-functions/R/get-snodas.R"))

#import UMRB outline for clipping and watershed for aggregating
UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#dates for snowdepth change
dates = c(as.Date(Sys.Date()):as.Date(Sys.Date()-7)) %>%
  as.Date(., origin = "1970-01-01")

#get todays date meta data to evalaute standardized dates
today = c(as.Date(Sys.Date())) %>%
  as.Date(., origin = "1970-01-01") %>%
  as_tibble() %>%
  mutate(year = lubridate::year(value), 
         day = lubridate::day(value),
         month = lubridate::month(value))%>%
  dplyr::select(year,month,day) 

#dates for standardized swe metric
standardized_dates = tibble(year = today$year:2004,
                            day = today$day,
                            month = today$month) %>%
  mutate(date = as.Date(paste(year, month, day, sep = '-')))

##########################################################
################### SNOW DEPTH CHANGE ####################
##########################################################

print('Starting SNODAS download and GDAL proccessing')

#download all appropriate data
get_snodas(c(dates, standardized_dates$date[2:length(standardized_dates$date)]))

print('Completed SNODAS download and GDAL proccessing')

print('check 1')

#get processed files
files = list.files(paste0(snodas.dir, "processed/snow_depth/"), full.names = T)

#compute time
time = files %>%
  fdates() %>%
  as.Date(., format = "%Y%m%d")

#process snodas data function
process_raster = function(date){
  raster_temp = raster(files[which(time == date)]) %>%
    crop(., extent(UMRB)) %>%
    mask(., UMRB) /1000 * 39.3701
  return(raster_temp)
}

#process rasters and compute snow depth changes
today = process_raster(Sys.Date())
delta_1 = process_raster(Sys.Date()) - process_raster(Sys.Date()-1)
delta_3 = process_raster(Sys.Date()) - process_raster(Sys.Date()-3)
delta_7 = process_raster(Sys.Date()) - process_raster(Sys.Date()-7)

print('check 2')


writeRaster(today, filename = paste0(snodas.dir, "processed/delta_snow_depth/current_depth_in.tif"), overwrite=TRUE)
writeRaster(delta_1, filename = paste0(snodas.dir,"processed/delta_snow_depth/delta_1_depth_in.tif"), overwrite=TRUE)
writeRaster(delta_3, filename = paste0(snodas.dir,"processed/delta_snow_depth/delta_3_depth_in.tif"), overwrite=TRUE)
writeRaster(delta_7, filename = paste0(snodas.dir,"processed/delta_snow_depth/delta_7_depth_in.tif"),overwrite=TRUE)

##########################################################
#################### STANDARDIZED SWE ####################
##########################################################

process_raster_standardize = function(file){
  raster_temp = raster(file) %>%
    crop(., extent(UMRB)) %>%
    mask(., UMRB) 
  return(raster_temp)
}

standardized_input = list.files(paste0(snodas.dir, '/processed/swe'), full.names = T) %>%
  as_tibble() %>%
  filter(stringr::str_detect(value, paste(standardized_dates$year, sprintf("%02d",standardized_dates$month), sprintf("%02d",standardized_dates$day),
                                          sep = '', collapse = '|'))) %>%
  as.list() %$%
  value %>%
  lapply(., process_raster_standardize) %>%
  brick()

print('check 3')


#calucalte time integrated precip sum
swe_vec = data.frame(matrix(nrow = length(values(standardized_input[[1]])), ncol = nlayers(standardized_input)))
for(i in 1:nlayers(standardized_input)){
  swe_vec[,i] = values(standardized_input[[i]])
}

#replace 0 values with very low value gamma cant take 0
swe_vec[swe_vec == 0] = 0.001

library(doParallel)
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

gc()
current_standardized_swe = parApply(cl, swe_vec, 1, FUN = spi_fun)
gc()

stopCluster(cl)

print('check 4')


#populate spatial template with data
standardized_swe_raster = standardized_input[[1]]
values(standardized_swe_raster) = current_standardized_swe

#import drought metric template for resampling to 4km
template = raster(paste0(export.dir, 'spi/current_spi_30.tif'))

#resmple to 4km
standardized_swe_raster_resampled = resample(standardized_swe_raster, template, method="bilinear")

print('check 5')


###############################3

#write it out 
writeRaster(standardized_swe_raster_resampled, paste0(export.dir, 'snodas/processed/standardized_swe/current_snodas_swe_standardized.tif'),
            overwrite = T)

#write out time meta 
write.csv(data.frame(time = standardized_dates$date[1]), paste0(export.dir, 'snodas/processed/standardized_swe/time.csv'))

########################## clean up data ##################################

do.call(file.remove, list(list.files(paste0(export.dir, "snodas/processed/swe/"), full.names = TRUE)))
do.call(file.remove, list(list.files(paste0(export.dir, "snodas/processed/snow_depth/"), full.names = TRUE)))


print('check 6')
