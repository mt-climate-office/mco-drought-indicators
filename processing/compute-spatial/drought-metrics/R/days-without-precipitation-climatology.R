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
time_without = time[(length(time$day)-((364*2)+1)):length(time$day),]
base_without = raster_precip_spatial_clip[[(length(time$day)-((364*2)+1)):length(time$day)]]

matrix_without = data.frame(matrix(nrow = length(values(base_without[[1]])), ncol = length(nlayers(base_without))))
for(i in 1:nlayers(base_without)){
  matrix_without[,i] = values(base_without[[i]])
}

cl = makeCluster(3)
registerDoParallel(cl)

thresh = c(0.1, 0.25, 0.5)

for(i in 1:length(thresh)){
  val = thresh[i] * 25.4

  clusterExport(cl,'val')

  days_without = parApply(cl,matrix_without, 1, FUN = function(x){library(dplyr); which(x %>% rev > val)[1]})

  days_without_rast = base_without[[1]]
  values(days_without_rast) = days_without

  plot(days_without_rast, terrain.colors(20), zlim = c(0,50))
  lines(UMRB)
  plot(base_without[[nlayers(base_without)-9]])

  writeRaster(days_without_rast, paste0('/home/zhoylman/temp/days_since_', thresh[i],'_in_event.tif'))
}

stopCluster(cl)