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

#compute water year
water_year = (length(time$day) - which(time$day == "10-01")[length(which(time$day == "10-01"))])
year_to_date = (length(time$day) - which(time$day == "01-01")[length(which(time$day == "01-01"))])

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
  
  #sum and mask precip in parellel
  raster_precip_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = sum(raster_precip_spatial_clip[[slice_vec[group_by_vec == i]]])
    mask(temp, UMRB)
  }

  #calucalte time integrated precip sum
  integrated_precip = data.frame(matrix(nrow = length(values(raster_precip_clipped[[1]])), ncol = length(unique(group_by_vec))))
  for(i in 1:length(unique(group_by_vec))){
    integrated_precip[,i] = values(raster_precip_clipped[[i]])
  }

  current_percent_of_normal = parApply(cl,integrated_precip, 1, FUN = percent_of_normal)
  current_deviation_from_normal = parApply(cl,integrated_precip, 1, FUN = deviation_from_normal)
  current_percentile = parApply(cl,integrated_precip, 1, FUN = percentile_inverse)
  current_raw = parApply(cl,integrated_precip, 1, FUN = raw_amount)
  
  #stop parellel cluster
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current percent_of_normal values
  percent_of_normal_map = raster_precip_clipped[[1]]
  deviation_from_normal_map = raster_precip_clipped[[1]]
  percentile_map = raster_precip_clipped[[1]]
  raw_map = raster_precip_clipped[[1]]

  #allocate curent percent_of_normal values to spatial template
  values(percent_of_normal_map) = current_percent_of_normal
  values(deviation_from_normal_map) = current_deviation_from_normal
  values(percentile_map) = current_percentile
  values(raw_map) = current_raw
  
  #define path for map export
  vars = c("current_percent_of_normal_", "current_deviation_from_normal_mm_", "current_percentile_", "current_raw_")
  
  path_file = list()
  
  for(v in 1:length(vars)){
    #define path for map export
    path_file[[v]] = paste0(export.dir, "precipitation/", vars[v],
                      as.character(time_scale[t]),".tif")
    
    # wateryear and year to date file name
    if(t > (length(time_scale)-2)){
      if(t == (length(time_scale)-1)){
        path_file[[v]] = paste0(export.dir, "precipitation/", vars[v],
                          "water_year",".tif", sep = "")
      }
      if(t == (length(time_scale))){
        path_file[[v]] = paste0(export.dir, "precipitation/", vars[v],
                          "year_to_date",".tif", sep = "")
      }
    }
  }
  
  #write GeoTiff
  maps = list(percent_of_normal_map, deviation_from_normal_map, percentile_map, raw_map)
  for(i in 1:4){
    writeRaster(maps[[i]], path_file[[i]], format = "GTiff", overwrite = T)
  }
}

#write time out
out.time = data.frame(time = substr(time$datetime[length(time$datetime)],1,10))
write.csv(out.time, paste0(export.dir, "precipitation/time.csv"))

############################################
############ Days Without P ################
############################################

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
  writeRaster(days_without_rast, paste0(export.dir, 'precipitation/days_since_', thresh[i],'_in_event.tif'), overwrite = T)
  writeRaster(percentile_without_rast, paste0(export.dir, 'precipitation/percentile_since_', thresh[i],'_in_event.tif'), overwrite = T)
  print(i)
}
stopCluster(cl)