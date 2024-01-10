#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

#import the remote data
raster_pet = brick("[FillMismatch]http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", var = "daily_mean_reference_evapotranspiration_grass")
proj4string(raster_pet) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#import UMRB outline for clipping a
UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#clip precip and PET grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_pet_spatial_clip = crop(raster_pet, extent(UMRB))

#define time
time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_pet_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

#compute water year
water_year = (length(time$day) - which(time$day == "10-01")[length(which(time$day == "10-01"))])+1
year_to_date = (length(time$day) - which(time$day == "01-01")[length(which(time$day == "01-01"))])+1

#designate time scale
time_scale = c(15,30,45,60,90,120,180,365,730, water_year, year_to_date)

for(t in 1:length(time_scale)){
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
  
  #sum and mask PET in parellel
  raster_pet_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = sum(raster_pet_spatial_clip[[slice_vec[group_by_vec == i]]])
    mask(temp, UMRB)
  }
  
  integrated_pet = data.frame(matrix(nrow = length(values(raster_pet_clipped[[1]])), ncol = length(unique(group_by_vec))))
  for(i in 1:length(unique(group_by_vec))){
    integrated_pet[,i] = values(raster_pet_clipped[[i]])
  }
  
  #clusterCall(cl, function() {source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))})
  current_eddi = parApply(cl,integrated_pet, 1, FUN = eddi_fun)
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current eddi values
  eddi_map = raster_pet_clipped[[1]]
  
  #allocate curent spi values to spatial template and add metadata of last time stamp used
  values(eddi_map) = current_eddi
  metadata(eddi_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  
  #define path to export and wrtie GEOtiff
  path_file = paste0(export.dir, "eddi/current_eddi_",
                    as.character(time_scale[t]),".tif", sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      path_file = paste0(export.dir, "eddi/current_eddi_",
                        "water_year",".tif")
    }
    if(t == (length(time_scale))){
      path_file = paste0(export.dir, "eddi/current_eddi_",
                        "year_to_date",".tif")
    }
  }
  
  writeRaster(eddi_map, path_file, format = "GTiff", overwrite = T)
}

#write time out
out.time = data.frame(time = substr(time$datetime[length(time$datetime)],1,10))
write.csv(out.time, paste0(export.dir, "eddi/time.csv"))

time = substr(time$datetime[length(time$datetime)],1,10)

#write simple txt
fileConn<-file(paste0(export.dir, "eddi/time.txt"))
writeLines(time, fileConn)
close(fileConn)
