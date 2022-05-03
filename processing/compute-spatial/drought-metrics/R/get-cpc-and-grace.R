#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

#import domain
UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#url for CPC soil moisture
url = "ftp://ftp.cpc.ncep.noaa.gov/GIS/USDM_Products/soil/percentile/daily/"

#pull in names of files
files = read.table(url) %>%
  dplyr::select(V9) %$%
  V9

#download most current data
download.file(paste0(url,files[length(files)]), 
              destfile = paste0(export.dir, 'soil-moisture/cpc-soil-moisture-conus.tif'))

#import and crop
soil_moisture = raster(paste0(export.dir, 'soil-moisture/cpc-soil-moisture-conus.tif'))%>%
  mask(., UMRB) %>%
  crop(., UMRB)

#write out clipped raster
writeRaster(soil_moisture, paste0(export.dir, 'soil-moisture/current-cpc-soil-moisture.tif'), overwrite = T)

#remove conus file 
system(paste0('rm ', export.dir, 'soil-moisture/cpc-soil-moisture-conus.tif'))

#write out time
out.time = data.frame(time = gsub("[^[:digit:]]", "",  files[length(files)]) %>%
                    as.Date(., format = '%Y%m%d'))
write.csv(out.time, paste0(export.dir, "soil-moisture/time.csv"))

########################################################## 
######################### GRACE ##########################
########################################################## 

#download the current grace data
grace_current = download.file('https://nasagrace.unl.edu/data/current/gws_perc_0125deg_US_current.tif', 
                              destfile = paste0(export.dir, 'grace/grace-groundwater-drought.tif'))

#import and crop
grace_data = raster(paste0(export.dir, 'grace/grace-groundwater-drought.tif'))%>%
  mask(., UMRB) %>%
  crop(., UMRB)

writeRaster(grace_data, paste0(export.dir, 'grace/current-grace-groundwater-drought.tif'), overwrite = T)

system(paste0('rm ', paste0(export.dir, 'grace/grace-groundwater-drought.tif')))

time = readLines('https://nasagrace.unl.edu/data/current/sfsm_perc_0125deg_US_current.tif.xml', skip = 2)[2] %>%
  str_extract(., "\\d{8}") %>%
  as.Date(., format = '%Y%m%d')
  
#write time out
out.time = data.frame(time = time -1)
write.csv(out.time, paste0(export.dir, "grace/time.csv"))

#write simple txt
fileConn<-file(paste0(export.dir, "grace/time.txt"))
writeLines((time -1) %>% as.character(), fileConn)
close(fileConn)
