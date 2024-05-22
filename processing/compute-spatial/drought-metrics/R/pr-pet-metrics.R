#define directories for I/O
export.dir = '~/mco-drought-indicators-data-conus/'
archive.dir = '~/mco-drought-indicators-data-archive-conus/'

# import ancillary functions
source('~/mco-drought-indicators/processing/ancillary-functions/R/load-libs.R')
source("~/mco-drought-indicators/processing/ancillary-functions/R/drought-functions.R")

###############################################################################
######################### Import Precipitation Data ###########################
###############################################################################

#import pr data (precipitation in mm)
#import the historical pr gridmet data
historical_pr = list.files('~/mco-drought-indicators-data/gridmet/pr', full.names = T) %>%
  rast()

#import the remote current year data
current_pr = dap(URL = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", 
                    varname = "precipitation_amount",
                    startDate = '2024-01-01',
                    #compute the most current date in current data (will change each day and depends on when request is made)
                    endDate = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", 
                                    varname = "precipitation_amount") %>%
                      names() %>%
                      tail(1) %>%
                      substr(.,2,100) %>%
                      as.numeric() + as.Date('1900-01-01')) %>%
  rast() 

#redefine crs with full GEOGCRS - the crs doesnt flow well from CDF to rast
crs(current_pr) <- 'epsg:4326'

#merge historical and currect data
gridmet_pr = rast(list(historical_pr, current_pr))

###############################################################################
############################## Import PET Data ################################
###############################################################################

#import pr data (precipitation in mm)
#import the historical pr gridmet data
historical_pet = list.files('~/mco-drought-indicators-data/gridmet/pet', full.names = T) %>%
  rast()

#import the remote current year data
current_pet = dap(URL = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", 
                 varname = "daily_mean_reference_evapotranspiration_grass",
                 startDate = '2024-01-01',
                 #compute the most current date in current data (will change each day and depends on when request is made)
                 endDate = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", 
                                 varname = "daily_mean_reference_evapotranspiration_grass") %>%
                   names() %>%
                   tail(1) %>%
                   substr(.,2,100) %>%
                   as.numeric() + as.Date('1900-01-01')) %>%
  rast() 

#redefine crs with full GEOGCRS - the crs doesnt flow well from CDF to rast
crs(current_pet) <- 'epsg:4326'

#merge historical and currect data
gridmet_pet = rast(list(historical_pet, current_pet))

###############################################################################
############################ Compute time vectors #############################
###############################################################################

#define time
time = tibble(datetime = append(as.Date(historical_pr %>%
                                   names() %>%
                                   readr::parse_number(), origin="1900-01-01"),
                                names(current_pr)),
              day = strftime(datetime,"%m-%d"))

#compute the timescale length for water year and year to date
water_year = (length(time$day) - which(time$day == "10-01")[length(which(time$day == "10-01"))])
year_to_date = (length(time$day) - which(time$day == "01-01")[length(which(time$day == "01-01"))])

#designate time scale
time_scale = c(15,30,40,60,90,180,365,730, water_year, year_to_date)

#loop through timescales
for(t in 1:length(time_scale)){
  #compute indexes for time breaks
  aggregation_vecs = compute_timescales(time, time_scale[t])
  
  #compute summation grids
  #precipitation first
  precip_sums = unique(aggregation_vecs$group_by_vec) %>%
    purrr::map(function(i){
      temp = terra::app(
        gridmet_pr[[aggregation_vecs$slice_vec[aggregation_vecs$group_by_vec == i]]], 
        fun = 'sum')
      return(temp)
    }) %>%
    terra::rast()
  
  #now pet
  pet_sums = unique(aggregation_vecs$group_by_vec) %>%
    purrr::map(function(i){
      temp = terra::app(
        gridmet_pet[[aggregation_vecs$slice_vec[aggregation_vecs$group_by_vec == i]]], 
        fun = 'sum')
      return(temp)
    }) %>%
    terra::rast()
  
  #compute the water balance
  water_balance = precip_sums - pet_sums
  
  #compute precipitation based metrics
  spi = terra::app(precip_sums, gamma_fit_spi, cores = 10)
  precip_percentile = terra::app(precip_sums, compute_percentile, cores = 10)
  precip_deviation_from_normal = terra::app(precip_sums, deviation_from_normal, cores = 10)
  
  #compute pet based metrics
  eddi = terra::app(pet_sums, eddi_fun, cores = 10)
  
  #deficit based metrics
  spei = terra::app(water_balance, glo_fit_spei, cores = 10)
  
  #plot check if you wish
  #plot(spei, breaks = c(-Inf, -2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3, 1.6, 2, Inf), col = colorspace::diverge_hsv(11) %>% rev())
  
  ###############################################################################
  ############################## Export Results #################################
  ###############################################################################
  
  #compute current time 
  current_time = time$datetime[max(aggregation_vecs$first_date_breaks)]
  
  #store results in a list to loop export
  metrics = list(spi, 
                 precip_percentile,
                 precip_deviation_from_normal,
                 eddi,
                 spei
  )
  
  metric_names = c('spi',
                   'precipitation_percentile',
                   'precipitation_deviation_from_normal',
                   'eddi',
                   'spei'
  )
  
  metric_subdir_name = c('spi',
                         'precipitation',
                         'precipitation',
                         'eddi',
                         'spei'
  )
  
  time_scale_name = as.character(time_scale[t])
  
  # water year and year to date timescale special case
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      time_scale_name = 'water_year'
    }
    if(t == (length(time_scale))){
      time_scale_name = 'year_to_date'
    }
  }
  
  #loop though metrics, export away!
  for(m in 1:length(metrics)){
    #define path for map export
    path_file = paste0(export.dir, metric_subdir_name[m], "/current_",
                       metric_names[m],'_',time_scale_name,".tif")
    
    path_file_archive = paste0(archive.dir, metric_subdir_name[m], "/",
                               str_extract_all(current_time, "\\(?[0-9,.]+\\)?") %>% 
                                 bind() %>% 
                                 str_flatten(), 
                               '_', metric_names[m],'_', time_scale_name,".tif")
    
    #write GeoTiff
    terra::writeRaster(metrics[[m]], path_file, overwrite = T)
    terra::writeRaster(metrics[[m]], path_file_archive, overwrite = T)
  }
}

#write time out
out.time = data.frame(time = substr(time$datetime[length(time$datetime)],1,10))
write.csv(out.time, paste0(export.dir, "spi/time.csv"))
time = substr(time$datetime[length(time$datetime)],1,10)

#write simple txt
fileConn<-file(paste0(export.dir, "spi/time.txt"))
writeLines(time, fileConn)
close(fileConn)
