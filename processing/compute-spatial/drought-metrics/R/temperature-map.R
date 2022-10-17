#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

#import remote data
raster_temp = brick("[FillMismatch]http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc", var= 'daily_maximum_temperature')
proj4string(raster_temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#import UMRB outline for clipping and watershed for aggregating
UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#clip temp grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_temp_spatial_clip = crop(raster_temp, extent(UMRB))

#define time
time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_temp_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

#compute water year
water_year = (length(time$day) - which(time$day == "10-01")[length(which(time$day == "10-01"))])+1
year_to_date = (length(time$day) - which(time$day == "01-01")[length(which(time$day == "01-01"))])+1

#designate time scale
time_scale = c(15,30,60,90,180,365, water_year, year_to_date)

for(t in 1:length(time_scale)){
  #compute indexes for time breaks
  first_date_breaks = which(time$day == time$day[length(time$datetime)])
  second_date_breaks = first_date_breaks-(time_scale[t]-1)
  
  #if there are negative indexes remove last year (incomplete data range)
  #change this to remove all indexes from both vectors that are negative
  if(!all(second_date_breaks < 0)){
    pos_index = which(second_date_breaks > 0)
    first_date_breaks = first_date_breaks[c(pos_index)]
    second_date_breaks = second_date_breaks[c(pos_index)]
  }
  
  #create slice vectors and group by vectors
  for(j in 1:length(first_date_breaks)){
    if(j == 1){
      slice_vec = seq(second_date_breaks[j],first_date_breaks[j], by = 1)
      group_by_vec = rep(j,(first_date_breaks[j] - second_date_breaks[j]+1))
    }
    else{
      slice_vec = append(slice_vec, seq(second_date_breaks[j],first_date_breaks[j], by = 1))
      group_by_vec = append(group_by_vec, rep(j,(first_date_breaks[j] - second_date_breaks[j]+1)))
    }
  }
  
  #start cluster for parellel computing
  cl = makeCluster(7)
  registerDoParallel(cl)
  
  #sum and mask temp in parellel
  raster_temp_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = mean(raster_temp_spatial_clip[[slice_vec[group_by_vec == i]]], na.rm = T)
    mask(temp, UMRB)
  }

  #calucalte time integrated temp sum
  integrated_temp = data.frame(matrix(nrow = length(values(raster_temp_clipped[[1]])), ncol = length(unique(group_by_vec))))
  for(i in 1:length(unique(group_by_vec))){
    integrated_temp[,i] = values(raster_temp_clipped[[i]])
  }
  
  current_deviation_from_normal = parApply(cl,integrated_temp, 1, FUN = deviation_from_normal)
  current_percentile = parApply(cl,integrated_temp, 1, FUN = percentile_inverse)
  
  #stop parellel cluster
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current deviation_from_normal values
  deviation_from_normal_map = raster_temp_clipped[[1]]
  percentile_map = raster_temp_clipped[[1]]
  
  #allocate curent deviation_from_normal values to spatial template
  values(deviation_from_normal_map) = current_deviation_from_normal
  values(percentile_map) = current_percentile
  
  #define path for map export
  vars = c("current_deviation_from_normal_degC_", "current_percentile_")
  
  path_file = list()
  
  for(v in 1:length(vars)){
    #define path for map export
    path_file[[v]] = paste0(export.dir, "temperature/", vars[v],
                            as.character(time_scale[t]),".tif")
    
    # wateryear and year to date file name
    if(t > (length(time_scale)-2)){
      if(t == (length(time_scale)-1)){
        path_file[[v]] = paste0(export.dir, "temperature/", vars[v],
                               "water_year",".tif")
      }
      if(t == (length(time_scale))){
        path_file[[v]] = paste0(export.dir, "temperature/", vars[v],
                               "year_to_date",".tif")
      }
    }
  }
  
  #write GeoTiff
  maps = list(deviation_from_normal_map, percentile_map)
  for(i in 1:2){
    writeRaster(maps[[i]], path_file[[i]], format = "GTiff", overwrite = T)
  }
}

#write time out
out.time = data.frame(time = substr(time$datetime[length(time$datetime)],1,10))
write.csv(out.time, paste0(export.dir, "temperature/time.csv"))

time = substr(time$datetime[length(time$datetime)],1,10)

#write simple txt
fileConn<-file(paste0(export.dir, "temperature/time.txt"))
writeLines(time, fileConn)
close(fileConn)
