#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'
snodas.dir = paste0(export.dir, 'snodas/')

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))
source(paste0(git.dir,"/processing/ancillary-functions/R/get-snodas.R"))

`%notin%` = Negate(`%in%`)

########################## clean up data ##################################
do.call(file.remove, list(list.files(paste0(export.dir, "snodas/processed/swe/"), full.names = TRUE)))
do.call(file.remove, list(list.files(paste0(export.dir, "snodas/processed/snow_depth/"), full.names = TRUE)))

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
    mask(., UMRB) /1000 * 39.3701 #m to in
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

rm(today, delta_1, delta_3, delta_7)
gc()
gc()
print('cleaned snow depth change workspace')

##########################################################
#################### STANDARDIZED SWE ####################
##########################################################

template = today

process_raster_standardize = function(file){
  raster_temp = raster(file) %>%
    crop(., extent(UMRB)) %>%
    mask(., UMRB) #%>%
    #raster dimentions change through time. 
    #raster::resample(., template, method = 'ngb')
  return(raster_temp)
}

standardized_input = list.files(paste0(snodas.dir, '/processed/swe'), full.names = T) %>%
  as_tibble() %>%
  filter(stringr::str_detect(value, paste(standardized_dates$year, sprintf("%02d",standardized_dates$month), sprintf("%02d",standardized_dates$day),
                                          sep = '', collapse = '|'))) %>%
  as.list() %$%
  value %>%
  lapply(., process_raster_standardize) %>%
  #lapply(., resample, y = template, method = 'ngb') %>%
  brick()

print('check 3')


#calucalte time integrated precip sum
swe_vec = data.frame(matrix(nrow = length(values(standardized_input[[1]])), ncol = nlayers(standardized_input)))
for(i in 1:nlayers(standardized_input)){
  swe_vec[,i] = values(standardized_input[[i]])
}

#replace 0 values with very low value gamma cant take 0
swe_vec[swe_vec == 0] = 0.1

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

#remove locations with 0 SWE -- 
mask_ = standardized_input[[nlayers(standardized_input)]]
values(mask_) = ifelse(values(mask_) > 0, 1, NA)
standardized_swe_raster = standardized_swe_raster * mask_

#import drought metric template for resampling to 4km
template = raster(paste0(export.dir, 'spi/current_spi_30.tif'))

#resmple to 4km
standardized_swe_raster_resampled = resample(standardized_swe_raster, template, method="bilinear")

#print('check 5')

###############################3

#write it out
writeRaster(standardized_swe_raster, paste0(export.dir, 'snodas/processed/standardized_swe/current_snodas_swe_standardized.tif'),
            overwrite = T)

writeRaster(standardized_swe_raster_resampled, paste0(export.dir, 'snodas/processed/standardized_swe/current_snodas_swe_standardized_4km.tif'),
            overwrite = T)

#write out time meta
write.csv(data.frame(time = standardized_dates$date[1]), paste0(export.dir, 'snodas/processed/standardized_swe/time.csv'))

#write simple txt
fileConn<-file(paste0(export.dir, "snodas/processed/standardized_swe/time.txt"))
writeLines(standardized_dates$date[1] %>% as.character(), fileConn)
close(fileConn)

rm(standardized_swe_raster, current_standardized_swe, swe_vec, standardized_input)
gc()
gc()
print('cleaned standardized_swe workspace')
################################################################
###################### Hypsome-swe #############################
################################################################
library(terra)
library(elevatr)
library(ggplot2)

watersheds = st_read('/home/zhoylman/mco-drought-indicators/processing/base-data/processed/watersheds_umrb.shp') 
names = watersheds$HUC8
print('starting hypsome-swe')

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)
tictoc::tic()
#length(names)
#try for error handling
percent_of_ave_store = list()

try({
  foreach(i = 1:length(names), .packages=c('terra', 'dplyr', 'elevatr', 'ggplot2', 'progress'))%dopar%{
    roi = watersheds %>% filter(HUC8 == names[i])
    
    swe = data.frame(files = list.files('/home/zhoylman/mco-drought-indicators-data/snodas/processed/swe/', full.names = T)) %>%
      as_tibble() %>%
      mutate(time = gsub("\\D", "", files),
             date = as.Date(time, format = '%Y%m%d'),
             month = lubridate::month(date),
             day = lubridate::day(date)) %>%
      filter(month == lubridate::month(Sys.Date()) & day == lubridate::day(Sys.Date())) %$%
      files %>%
      lapply(., rast) %>%
      lapply(., terra::crop, roi %>% vect) %>%
      lapply(., terra::mask, roi %>% vect) %>%
      lapply(., terra::project, "epsg:5070")
    
    swe = swe %>%
      lapply(., terra::resample, swe[[length(swe)]], method="bilinear") %>%
      rast
    
    # summer = function(x){
    #   return((((values(x) %>% sum(., na.rm = T)) /
    #       (values(last) %>% sum(., na.rm = T)))*100))
    # }
    # years = 2004:2022
    # for(i in 1:nlyr(swe)){
    #   print(paste0(years[i], ' = ' , summer(swe[[i]]), ' % of 2022'))
    # }
    
    mean_swe = median(swe, na.rm = T)
    last = swe[[nlyr(swe)]]
    
    percent_of_ave = (((values(last) %>% sum(., na.rm = T)) /
                         (values(mean_swe) %>% sum(., na.rm = T)))*100) %>%
      round(.,  0)
    
    if(percent_of_ave == 'NaN'){
      percent_of_ave = 0
    }
    
    if(percent_of_ave > 300){
      
      percent_of_ave = '> 300'
    }
    
    percent_of_ave_store[[i]] = percent_of_ave
    
    dem = get_elev_raster(roi, z = 9) %>%
      rast %>%
      crop(., roi %>% vect) %>%
      mask(., roi %>% vect) %>%
      project(., mean_swe, method = 'bilinear')
    
    data = data.frame(dem = values(dem),
                      mean_swe_m = (((values(mean_swe)/1000) * (res(mean_swe)[1] * res(mean_swe)[2]))),
                      year_2021 = (((values(last)/1000) * (res(mean_swe)[1] * res(mean_swe)[2])))) %>%
      `colnames<-`(c('dem',  paste0('Median Climatology (2004 - ', lubridate::year(Sys.Date()), ')'), lubridate::year(Sys.Date()))) %>%
      as_tibble() %>%
      tidyr::drop_na() %>%
      mutate(quantile = .bincode(dem, quantile(dem, seq(0.01,1,by = 0.01))),
             lin.bins = .bincode(dem, seq(min(dem, na.rm = T)-1, max(dem, na.rm = T)+1, length.out = 50))) %>%
      tidyr::pivot_longer(names_to = 'name', cols = c(`Median Climatology (2004 - 2024)`, `2024`)) %>%
      group_by(lin.bins, name) %>%
      summarise(n = length(value),
                sum = sum(value, na.rm = T)) %>%
      ungroup() %>%
      left_join(., data.frame(dem = values(dem) * 3.28084,
                              mean_swe_m = (((values(mean_swe)/1000) * (res(mean_swe)[1] * res(mean_swe)[2]))),
                              year_2021 = (((values(last)/1000) * (res(mean_swe)[1] * res(mean_swe)[2])))) %>%
                  `colnames<-`(c('dem', paste0('Median Climatology (2004 - ', lubridate::year(Sys.Date()), ')'), lubridate::year(Sys.Date()))) %>%
                  as_tibble() %>%
                  tidyr::drop_na() %>%
                  mutate(quantile = .bincode(dem, quantile(dem, seq(0.01,1,by = 0.01))),
                         lin.bins = .bincode(dem, seq(min(dem, na.rm = T), max(dem, na.rm = T), length.out = 50))) %>%
                  tidyr::pivot_longer(names_to = 'name', cols = c(dem)) %>%
                  group_by(lin.bins, name) %>%
                  summarise(mean_dem = mean(value, na.rm = T)) %>%
                  ungroup() , by = 'lin.bins') %>%
      dplyr::select(-name.y) %>%
      mutate(name.x = forcats::fct_relevel(name.x , c(lubridate::year(Sys.Date()) %>% as.character(), paste0('Median Climatology (2004 - ', lubridate::year(Sys.Date()), ')')))) %>%
      bind_rows(data.frame(lin.bins = c(51,51, 0, 0),
                           name.x = c(lubridate::year(Sys.Date()) %>% as.character(),  paste0('Median Climatology (2004 - ', lubridate::year(Sys.Date()), ')'),
                                      lubridate::year(Sys.Date()) %>% as.character(),  paste0('Median Climatology (2004 - ', lubridate::year(Sys.Date()), ')')),
                           n = c(0,0,0,0),
                           sum = c(0,0,0,0),
                           mean_dem = c(max(.$mean_dem, na.rm = T)+2, max(.$mean_dem, na.rm = T)+2, min(.$mean_dem, na.rm = T)-2, min(.$mean_dem, na.rm = T)-2))) %>%
      arrange(lin.bins)
    
    #compute # of average at all elevations
    percent_of_normal_elev = data %>%
      tidyr::pivot_wider(names_from = c('name.x'), values_from = sum) %>%
      mutate(`Percent of Average` = (`2024`/`Median Climatology (2004 - 2024)`)*100,
             `Percent of Average` = ifelse(`Percent of Average` > 300, 300, `Percent of Average`),
             `Percent of Average` = ifelse(is.na(`Percent of Average`) & n > 0, 0, `Percent of Average`))
    
    
    center_of_mass = data %>%
      dplyr::group_by(name.x) %>%
      summarize(com = weighted.mean(mean_dem, sum))
    
    n_int_digits = function(x) {
      result = floor(log10(abs(x)))
      result[!is.finite(result)] = 0
      result
    }
    
    magnitude = max(data$sum) %>% n_int_digits
    
    axis_tics = seq(-max(data$sum), max(data$sum), length.out = 5) %>% plyr::round_any(., 1*10^(magnitude-1))
    
    climatology = data %>%
      filter(name.x == paste0('Median Climatology (2004 - ', lubridate::year(Sys.Date()), ')'))
    
    current = data %>%
      filter(name.x == lubridate::year(Sys.Date()))
    
    plot = ggplot()+
      geom_polygon(data = climatology, aes(y = mean_dem, x = sum, color = name.x, fill = name.x), alpha = 0.6) +
      #geom_polygon(data = climatology, aes(y = mean_dem, x = -sum, color = name.x, fill = name.x), alpha = 0.6) +
      geom_polygon(data = current, aes(y = mean_dem, x = sum, color = name.x, fill = name.x), alpha = 0.3) +
      #geom_polygon(data = current, aes(y = mean_dem, x = -sum, color = name.x, fill = name.x), alpha = 0.3) +
      geom_point(data = center_of_mass, aes(y = com, x = 0, fill = name.x), color = 'black', shape = 23, size = 4)+
      theme_bw(base_size = 14)+
      theme(legend.position = 'bottom')+
      scale_x_continuous(breaks=axis_tics,
                         labels= scales::comma(abs(axis_tics)))+
      labs(x = 'Snow Water Equivalent (m³)', y = 'Elevation (ft)')+
      scale_fill_manual(values = c('blue', 'darkgrey'))+
      guides(fill = guide_legend(title.position = "left", title = NULL))+
      scale_color_manual(values = c('blue', 'darkgrey'), guide = F)+
      #ggtitle(paste0(paste0('Hypsome-SWE for ', roi$NAME, ' (HUC8: ', roi$HUC8, ')\n',max(time), ' (', percent_of_ave, '% of Normal)')))+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.margin=unit(c(1,1,1,1),"cm"))+
      theme(legend.title.align=0.5) 
    
    title = cowplot::ggdraw() + cowplot::draw_label(paste0(paste0('Hypsome-SWE for ', roi$NAME, ' (HUC8: ', roi$HUC8, ')\n',max(time), ' (', percent_of_ave, '% of Normal)')), fontface='bold')
    
    plot_percent = ggplot(data = percent_of_normal_elev, aes(x = `Percent of Average`, y = mean_dem))+
      geom_path()+
      geom_vline(xintercept = 100, linetype = 'dashed')+
      theme_bw(base_size = 14)+
      labs(x = 'Percent of Average SWE', y = NULL)+
      theme(plot.margin=unit(c(1,1,2.6,0),"cm"),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_x_continuous(breaks = c(0,100,200,300), labels = c("0%", '100%', '200%', ">300%"), limits = c(0,300))
    
    final = cowplot::plot_grid(title, cowplot::plot_grid(plot, plot_percent, align = 'h', rel_widths = c(1,0.5)), ncol = 1, rel_heights = c(0.1,1))
    
    ggsave(final, file = paste0('/home/zhoylman/mco-drought-indicators-data/snodas/plots/hypsome-swe-', roi$HUC8, '.png'), width = 8, height = 8, units = 'in')
    rm(plot, current, climatology, percent_of_normal_elev, data, dem, swe, mean_swe, last, roi)
    gc()
    percent_of_ave_store
  }
})

tictoc::toc()
print('finished hypsome-swe')

stopCluster(cl)

# compute percent of normal swe for each huc
swe = data.frame(files = list.files('/home/zhoylman/mco-drought-indicators-data/snodas/processed/swe/', full.names = T)) %>%
  as_tibble() %>%
  mutate(time = gsub("\\D", "", files),
         date = as.Date(time, format = '%Y%m%d'),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(month == lubridate::month(Sys.Date()) & day == lubridate::day(Sys.Date())) %$%
  files %>%
  lapply(., rast) %>%
  lapply(., terra::project, "epsg:5070")

swe = swe %>%
  lapply(., terra::resample, swe[[length(swe)]], method="bilinear") %>%
  rast

mean_swe = median(swe, na.rm = T)
last = swe[[nlyr(swe)]]

percent_median = (last/mean_swe)*100

percent_median_huc = exactextractr::exact_extract(percent_median, watersheds, 'median')

percent_median_huc[percent_median_huc > 300] = 300

watersheds_current = watersheds %>% 
  mutate(percent_of_normal = percent_median_huc %>%
           round(., 0))

write_sf(watersheds_current, '/home/zhoylman/mco-drought-indicators-data/snodas/current_watershed_percent/current_snodas_percent.geojson', delete_dsn = T)

########################## snodas timeseries ###############################

current_swe =  data.frame(files = list.files('/home/zhoylman/mco-drought-indicators-data/snodas/processed/swe/', full.names = T)) %>%
  as_tibble() %>%
  mutate(time = gsub("\\D", "", files),
         date = as.Date(time, format = '%Y%m%d')) %>%
  filter(date == Sys.Date())

# get start year based on water year (water year)
base_month = Sys.Date() %>% lubridate::month()
if(base_month %in% c(1:9)){
  base_year = Sys.Date() %>% lubridate::year() - 1
} else {
  base_year = Sys.Date() %>% lubridate::year()
}

library(exactextractr)
timeseries = read_csv('/home/zhoylman/mco-drought-indicators-data/snodas/timeseries/snodas_watershed_sum_swe_m3.csv')
today_rast = (rast(current_swe$files)/1000) %>% #mm to m
  crop(., watersheds) %>%
  mask(., watersheds) %>%
  terra::project(., "epsg:5070")

#update time series data is needed
if(!any(timeseries$time %in% Sys.Date()) == T){
  today_extract = exact_extract(today_rast, watersheds, fun = 'sum')  * (res(today_rast)[1] * res(today_rast)[2])
  
  today_extract_tbl = as_tibble(today_extract) %>%
    t() %>%
    as_tibble()
  
  colnames(today_extract_tbl) = watersheds$HUC8
  
  today_extract_tbl = today_extract_tbl %>%
    mutate(time = Sys.Date())
  
  timeseries_append = bind_rows(timeseries, today_extract_tbl)
  
  write_csv(timeseries_append, '/home/zhoylman/mco-drought-indicators-data/snodas/timeseries/snodas_watershed_sum_swe_m3.csv')
} else {
  timeseries_append = timeseries
}

yday.waterYear = function(x, start.month = 10L){
  day = day(x)
  month = month(x)
  #dont want yday to go from 1 - 366, rather to 365
  new_date = make_date(2024, month, day)
  start.yr = year(new_date) - (month(new_date) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(new_date - start.date + 1L)
}

base_year_waterYear = function(x){
  base_month = x %>% lubridate::month()
  if(base_month %in% c(1:9)){
    base_year = x %>% lubridate::year() - 1
  } else {
    base_year = x %>% lubridate::year()
  }
  return(base_year)
} 

#compute climatology
timeseries_append = timeseries_append %>%
  tidyr::pivot_longer(., cols = -c(time), values_to = 'swe') %>%
  mutate(water_year_yday = yday.waterYear(time)) %>%
  rowwise() %>%
  mutate(water_base_year = base_year_waterYear(time))

snodas_climatology = timeseries_append %>%
  filter(water_base_year %notin% base_year) %>%
  group_by(name, water_year_yday) %>%
  summarise(swe_q50 = quantile(swe, 0.5),
            swe_q05 = quantile(swe, 0.05),
            swe_q25 = quantile(swe, 0.25),
            swe_q75 = quantile(swe, 0.75),
            swe_q95 = quantile(swe, 0.95),
            swe_max = max(swe),
            swe_min = min(swe)) %>%
  mutate(time = as.Date(water_year_yday, origin = as.Date(paste0(base_year, '-09-30'), format = '%Y-%m-%d')))

this_year_swe = timeseries_append %>%
  filter(water_base_year == base_year)

plot_snodas = function(current_data, climatology_data, site_id_, base_year){
  data_select = current_data %>%
    dplyr::filter(name == site_id_)

  climatology_start_year = 2003

  climatology_data_select = climatology_data  %>%
    dplyr::filter(name == site_id_) %>%
    mutate(WY_date = time)

  site_meta = watersheds %>%
    dplyr::filter(HUC8 == site_id_)

  plot = ggplot()+
    ggtitle(paste0(str_to_title(site_meta$NAME), " (", as.Date(max(data_select$time)), ")"), 'Data Derived from SNODAS')+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q95, ymax = swe_max, fill = "95th - Max"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q75, ymax = swe_q95, fill = "75th - 95th"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q25, ymax = swe_q75, fill = "25th - 75th"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q05, ymax = swe_q25, fill = "5th - 25th"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_min, ymax = swe_q05, fill = "Min - 5th"), alpha = 0.25)+
    geom_line(data = climatology_data_select, aes(x = WY_date, y = swe_min), color = 'darkred', size = 0.75)+
    geom_line(data = climatology_data_select, aes(x = WY_date, y = swe_max), color = 'darkblue', size = 0.75)+
    geom_line(data = climatology_data_select, aes(x = WY_date, y = swe_q50, color = "Median"), size = 0.75)+
    geom_line(data = data_select, aes(x = time, y = swe, color = "Current"), size = 2)+
    scale_color_manual(name = "",values = c(
      'Median' = 'forestgreen',
      'Current' = 'black')) +
    scale_fill_manual(name = paste0('Percentiles\n(',climatology_start_year,' - 2022)'), values = c("darkblue","cyan","green","orange", 'darkred'),
                      breaks = c("95th - Max", "75th - 95th", "25th - 75th", "5th - 25th", "Min - 5th")) +
    ylab("Snow Water Equivalent (m³)")+
    xlab("Date")+
    theme_bw(base_size = 20)+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.title.align=0.5)
  
  return(plot)
}

for(i in 1:length(watersheds$HUC8)){
  #print(i)
  snodas_climatology_plot = plot_snodas(current_data = this_year_swe, 
                                   climatology_data = snodas_climatology, 
                                   site_id_ = watersheds$HUC8[i], 
                                   base_year = 2023)
  ggsave(snodas_climatology_plot, file = paste0('/home/zhoylman/mco-drought-indicators-data/snodas/plots/snodas-climatology-swe-', watersheds$HUC8[i], '.png'), 
         width = 10, height = 8, units = 'in')
}


########################## clean up data ##################################

do.call(file.remove, list(list.files(paste0(export.dir, "snodas/processed/swe/"), full.names = TRUE)))
do.call(file.remove, list(list.files(paste0(export.dir, "snodas/processed/snow_depth/"), full.names = TRUE)))


print('check 6')
