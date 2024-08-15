#devtools::install_github("mt-climate-office/usgs-discharge")
library(usgs.discharge)
library(dplyr)

future::plan(future::multisession, workers = 10)

#station meta
stations = get_gauges(clip_shp = usgs.discharge::domain)

#get discharge data
discharge = get_discharge_shp(stations)

#compute geojson 
discharge_shp = calc_discharge_anomalies(discharge)

#generate plotss
furrr::future_pmap(discharge, make_climatology_plot, out_dir = "/home/zhoylman/mco-drought-indicators-data/usgs/figures") 

timescales = c('today', '7', '14', '28')

for(i in timescales){
  temp = discharge_shp %>%
    dplyr::filter(time == i)
  
  print('delete old data')
  system(paste0('rm /home/zhoylman/mco-drought-indicators-data/usgs/geojson/usgs_', 
                i,
                '_current_percentiles.geojson'))
  print('write new data')
  sf::write_sf(temp, paste0('/home/zhoylman/mco-drought-indicators-data/usgs/geojson/usgs_', 
                                   i,
                                   '_current_percentiles.geojson'))
}
