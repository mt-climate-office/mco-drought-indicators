#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

#import remote data
raster_precip = brick("[FillMismatch]http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= 'precipitation_amount')
proj4string(raster_precip) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#import UMRB outline for clipping and watershed for aggregating
UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#clip precip grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(UMRB))

#define time
time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

## days without precip ###
#compute index of interest (most current day) for all years
index_of_interest = which(time$day == time$day[length(time$day)])
#select last 30 years for analysis  
index_of_interest = index_of_interest[(length(index_of_interest)-29):length(index_of_interest)]

cl = makeCluster(7)
registerDoParallel(cl)

temp_matrix = data.frame(matrix(nrow = length(values(raster_precip_spatial_clip[[1]])), ncol = 30))
temp_list = list(temp_matrix,temp_matrix,temp_matrix)
tictoc::tic()
for(s in 1:length(index_of_interest)){
  #search space is 2 years before the index of interest
  base_without = raster_precip_spatial_clip[[(index_of_interest[s]-(364*2)+1):index_of_interest[s]]] 
  #vecorize data
  matrix_without = data.frame(matrix(nrow = length(values(base_without[[1]])), ncol = (nlayers(base_without))))
  for(l in 1:nlayers(base_without)){
    matrix_without[,l] = values(base_without[[l]])
  }
  
  #find days since serrch val
  temp_list[[1]][,s] = parApply(cl,matrix_without, 1, FUN = function(x){library(dplyr); which(x %>% rev >= (0.1*25.4))[1]})
  temp_list[[2]][,s] = parApply(cl,matrix_without, 1, FUN = function(x){library(dplyr); which(x %>% rev >= (0.25*25.4))[1]})
  temp_list[[3]][,s] = parApply(cl,matrix_without, 1, FUN = function(x){library(dplyr); which(x %>% rev >= (0.5*25.4))[1]})
  
  print(s)
  gc()
}
tictoc::toc()


thresh = c(0.1,0.25,0.5)

for(i in 1:length(thresh)){
  #generate the raster of days days since
  days_without_rast = base_without[[1]] %>%
    `values<-`(temp_list[[i]][,30]) %>%
    `proj4string<-`("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #compute percentile of most recent days without
  percentile = parApply(cl,temp_list[[i]], 1, FUN = percentile_inverse)
  #generate the raster of percentile days since
  percentile_without_rast = base_without[[1]]%>%
    `values<-`(percentile)%>%
    `proj4string<-`("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #write out rasters
  writeRaster(days_without_rast, paste0('/home/zhoylman/temp/days_since_', thresh[i],'_in_event.tif'), overwrite = T)
  writeRaster(percentile_without_rast, paste0('/home/zhoylman/temp/percentile_since_', thresh[i],'_in_event.tif'), overwrite = T)
  print(i)
}
stopCluster(cl)

