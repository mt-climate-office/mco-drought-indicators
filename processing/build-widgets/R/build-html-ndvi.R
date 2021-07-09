#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#load all libraries for all apps
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#load custom functions
source(paste0(git.dir, '/processing/ancillary-functions/R/widget-functions.R'))
source(paste0(git.dir, '/processing/ancillary-functions/R/base-map.R'))

#define variable names to process
variable = c('NDVI Anomaly', 'NDVI Trend')
lower_variable = c('anom', 'trend')

#import counties
counties = st_read(paste0(git.dir, 'processing/base-data/processed/county_umrb.shp'))
watersheds = st_read(paste0(git.dir, 'processing/base-data/processed/watersheds_umrb.shp'))
tribal = st_read(paste0(git.dir, 'processing/base-data/processed/UMRB_tribal_lands_simple.geojson'))

#timescales 
timescales = c(7,15,30,60,90)

# define leaflet inputs
pal_anom = colorNumeric(c('#5c4033','#654321', '#ffffff','#90ee90', '#228B22'), -1.5:1.5, na.color = "transparent")
pal_trend = colorNumeric(c('#5c4033','#654321', '#ffffff','#90ee90', '#228B22'), c(-2.5,2.5), na.color = "transparent")

timescale_names = c("7 Day", "15 Day", "30 Day", "60 Day", "90 Day")

for(v in 1:length(variable)){
  #import data
  files = list.files(paste0(export.dir, 'ndvi/data/',lower_variable[v]), full.names = T) %>%
    as_tibble() %>%
    filter(stringr::str_detect(value, paste(timescales, sep = '', collapse = '|'))) 
  
  #reorder filtered vector
  data = files$value[sapply(timescales, function(x) { grep(x, files$value)})] %>%
    as_tibble() %>%
    as.list() %$%
    value %>%
    lapply(., raster) 
  
  # get current time 
  time = read_csv(paste0(export.dir, 'ndvi/data/',lower_variable[v], '/time.csv')) %$%
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
  
  if(lower_variable[v] == 'anom'){
    revalued_data = data %>%
      lapply(., revalue_raster_data, min = -1.5, max = 1.5)
    title = paste0(variable[v], '<br>', as.character(time))
  }
  
  if(lower_variable[v] == 'trend'){
    temp = data
    for(i in 1:length(timescales)){
      values(temp[[i]]) = (values(temp[[i]]) - 
                             mean(values(temp[[i]]), na.rm = T))/sd(values(temp[[i]]), na.rm = T)
    }
    revalued_data = temp %>%
      lapply(., revalue_raster_data, min = -2.5, max = 2.5)
    
    title = paste0('Standardized<br>',variable[v], '<br>', as.character(time))
  }
  
  #Implement the aggregation by geometry boundries once I figure out how to do this.
  
  # revalued_watershed_vals = watershed_vals %>%
  #   lapply(., revalue_vector_data, min = -2.5, max = 2.5)
  # 
  # revalued_county_vals = county_vals %>%
  #   lapply(., revalue_vector_data, min = -2.5, max = 2.5)
  # 
  # revalued_tribal_vals = tribal_vals %>%
  #   lapply(., revalue_vector_data, min = -2.5, max = 2.5)
  
  # make leaflet widgets
  if(lower_variable[v] == 'anom'){
    m_raster = build_html_raster(revalued_data, timescale_names, variable[v], title, pal_anom, legend_values = -1.5:1.5)
  }
  if(lower_variable[v] == 'trend'){
    m_raster = build_html_raster(revalued_data, timescale_names, variable[v], title, pal_trend, -2.5:2.5)
  }
  saveWidget(m_raster, paste0(export.dir, "widgets/m_raster_ndvi_", lower_variable[v], ".html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))
}
