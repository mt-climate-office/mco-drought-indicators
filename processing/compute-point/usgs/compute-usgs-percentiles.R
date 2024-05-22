#devtools::install_github("mt-climate-office/usgs-discharge")
library(usgs.discharge)
future::plan(future::multisession, workers = 10)

#station meta
stations = get_gauges(clip_shp = usgs.discharge::domain)

#get discharge data
discharge = get_discharge_shp(stations)

#compute geojson 
discharge_shp = calc_discharge_anomalies(discharge)

#generate plots
furrr::future_pmap(discharge, make_climatology_plot, out_dir = "~/mco-drought-indicators-data/usgs/figures") 

timescales = c('today', '7', '14', '28')

for(i in timescales){
  temp = discharge_shp %>%
    dplyr::filter(time == i)
  
  sf::write_sf(temp, paste0('~/mco-drought-indicators-data/usgs/geojson/usgs_', 
                                   i,
                                   '_current_percentiles.geojson'))
}
