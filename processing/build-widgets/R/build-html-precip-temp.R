#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#load all libraries for all apps
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#load custom functions
source(paste0(git.dir, '/processing/ancillary-functions/R/widget-functions.R'))
source(paste0(git.dir, '/processing/ancillary-functions/R/base-map.R'))

#define variable names to process
variable = c('Precipitation', 'Temperature')
lower_variable = c('precipitation', 'temperature')

#import counties
counties = st_read(paste0(git.dir, 'processing/base-data/processed/county_umrb.shp'))
watersheds = st_read(paste0(git.dir, 'processing/base-data/processed/watersheds_umrb.shp'))
tribal = st_read(paste0(git.dir, 'processing/base-data/processed/UMRB_tribal_lands_simple.geojson'))

#timescales 
timescales = c(15,30,60,90,180,365,'water_year', 'year_to_date')

# define leaflet inputs
pal_bins = colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                     domain = 0:100, bins = seq(0,100,10), na.color = "transparent")

pal_bins_reverse = colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                    domain = 0:100, bins = seq(0,100,10), na.color = "transparent")

timescale_names = c("15 Day","30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")

for(v in 1:length(variable)){
  #import data
  files = list.files(paste0(export.dir,lower_variable[v]), full.names = T) %>%
    as_tibble() %>%
    filter(stringr::str_detect(value, paste(timescales, sep = '', collapse = '|')),
           stringr::str_detect(value, 'current_percentile')) 
  
  #reorder filtered vector
  data = files$value[sapply(timescales, function(x) { grep(x, files$value)})] %>%
    as_tibble() %>%
    as.list() %$%
    value %>%
    lapply(., raster) 
  
  # get current time 
  time = read_csv(paste0(export.dir,lower_variable[v], '/time.csv')) %$%
    time[1]
  
  #Implement the aggregation by geometry boundries once I figure out how to do this.
  
  # #compute median values for each vector geometry
  # watershed_vals = data %>%
  #   lapply(., aggregate_by_vector, vector = watersheds)
  # 
  # county_vals = data %>%
  #   lapply(., aggregate_by_vector, vector = counties)
  # 
  # tribal_vals = data %>%
  #   lapply(., aggregate_by_vector, vector = tribal)
  # 
  # #compute labels 
  # labels_tribal = list()
  # for(i in 1:length(tribal_vals)){
  #   labels_tribal[[i]] <- sprintf(
  #     "<strong>%s</strong><br/>%s = %g<sup></sup>",
  #     tribal$GNIS_Name1, rep(variable[v],length(tribal$GNIS_Name1)), tribal_vals[[i]]
  #   ) %>% lapply(htmltools::HTML)
  # }
  # 
  # labels_watershed = list()
  # for(i in 1:length(watershed_vals)){
  #   labels_watershed[[i]] <- sprintf(
  #     "<strong>%s</strong><br/>%s = %g<sup></sup>",
  #     watersheds$NAME, rep(variable[v],length(watersheds$NAME)),  watershed_vals[[i]]
  #   ) %>% lapply(htmltools::HTML)
  # }
  # 
  # labels_county = list()
  # for(i in 1:length(county_vals)){
  #   labels_county[[i]] <- sprintf(
  #     "<strong>%s</strong><br/>%s = %g<sup></sup>",
  #     counties$NAME, rep(variable[v],length(counties$NAME)), county_vals[[i]]
  #   ) %>% lapply(htmltools::HTML)
  # }
  # 
  # revalue data
  
  revalued_data = data %>%
    lapply(., revalue_raster_data, min = 0.1, max = 99.9)
  
  #Implement the aggregation by geometry boundries once I figure out how to do this.
  
  # revalued_watershed_vals = watershed_vals %>%
  #   lapply(., revalue_vector_data, min = -2.5, max = 2.5)
  # 
  # revalued_county_vals = county_vals %>%
  #   lapply(., revalue_vector_data, min = -2.5, max = 2.5)
  # 
  # revalued_tribal_vals = tribal_vals %>%
  #   lapply(., revalue_vector_data, min = -2.5, max = 2.5)
  
  #define legend title name (HTML)
  title = paste0(variable[v], "<br>Percentile<br>", as.character(time))
  
  # make leaflet widgets
  if(variable[v] == 'Precipitation'){
    m_raster = build_html_raster(revalued_data, timescale_names, variable[v], title, pal_bins, legend_values = seq(0, 100, by = 10))
  }
  if(variable[v] == 'Temperature'){
    m_raster = build_html_raster(revalued_data, timescale_names, variable[v], title, pal_bins_reverse, legend_values = seq(0, 100, by = 10))
  }
  saveWidget(m_raster, paste0(export.dir, "widgets/m_raster_", lower_variable[v], ".html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))
}
