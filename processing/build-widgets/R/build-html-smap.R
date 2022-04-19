#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#load all libraries for all apps
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#load custom functions
source(paste0(git.dir, '/processing/ancillary-functions/R/widget-functions.R'))
source(paste0(git.dir, '/processing/ancillary-functions/R/base-map.R'))

#define variable names to process
variable = c('SMAP Subsurface Soil Moisture')
lower_variable = c('smap')

#import counties
counties = st_read(paste0(git.dir, 'processing/base-data/processed/county_umrb.shp'))
watersheds = st_read(paste0(git.dir, 'processing/base-data/processed/watersheds_umrb.shp'))
tribal = st_read(paste0(git.dir, 'processing/base-data/processed/UMRB_tribal_lands_simple.geojson'))

pal = colorNumeric(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), -2.5:2.5, na.color = "transparent")
pal_reverse = colorNumeric(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), -2.5:2.5, na.color = "transparent")
for(v in 1:length(variable)){
  #import data
  files = list.files(paste0(export.dir, 'smap/data'), full.names = T) %>%
    as_tibble() %>%
    filter(stringr::str_detect(value, paste('.tif', sep = '', collapse = '|'))) 
  
  #import raster
  data = files %>%
    lapply(., raster) 
  
  # get current time 
  time = read_csv(paste0(export.dir, 'smap/data/time.csv')) %$%
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
    lapply(., revalue_raster_data, min = -2.5, max = 2.5)
  
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
  title = paste0(variable[v], "<br>Anomoly<br>", as.character(time))
  
  library(RCurl)
  
  stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
    read_csv() %>%
    mutate(year = substr(`Start date`,9,13)) %>%
    filter(year <= '2018')
  
  mesonet = read_csv('/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/current_anom.csv') %>%
    filter(name %in% c('soilwc08', 'soilwc20'),
           station_key %in% stations$`Station ID`) %>%
    dplyr::select(datetime,name,anom,Longitude,Latitude) %>%
    mutate(anom = ifelse(anom < -2.5, -2.5, 
                         ifelse(anom > 2.5, 2.5, anom))) %>%
    tidyr::pivot_wider(names_from = name, values_from = anom) 
  
  # make leaflet widgets
  if(variable[v] == c('SMAP Subsurface Soil Moisture')){
    m_raster = build_html_raster(revalued_data, 'SMAP Subsurface Soil Moisture Anomaly', variable[v], title, pal, legend_values = -2.5:2.5) %>%
      addCircleMarkers(mesonet$Longitude, mesonet$Latitude,
                       radius = 10, stroke = TRUE, fillOpacity = 0.9,
                       color = "black", fillColor = pal(mesonet$soilwc08), group = "MT Mesonet 8in Anomaly") %>%
      addCircleMarkers(mesonet$Longitude, mesonet$Latitude,
                       radius = 10, stroke = TRUE, fillOpacity = 0.9,
                       color = "black", fillColor = pal(mesonet$soilwc20), group = "MT Mesonet 20in Anomaly") %>%
      leaflet::addLegend("topright", group = "MT Mesonet 8in Anomaly", pal = pal, values = -2.5:2.5,title = paste0("MT Mesonet<br>Soil Moisture<br>Anomaly<br>",
                                                                                                                   mesonet$datetime[1] %>% as.Date())) %>%
      # leaflet::addLegend("topright", group = "MT Mesonet 20in Anomaly", pal = pal, values = -2.5:2.5,title = paste0("MT Mesonet<br>Soil Moisture<br>Anomaly<br>",
      #                                                                                                               mesonet$datetime[1] %>% as.Date())) %>%
      addLayersControl(position = "topleft",
                       baseGroups = c('SMAP Subsurface Soil Moisture Anomaly'),
                       overlayGroups = c("MT Mesonet 8in Anomaly", "USDM", "States", "Weather", "Streets", "Counties", 'Watersheds', 'Tribal Lands'),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      leaflet::hideGroup(c("Watersheds", "Counties", "Streets", 'Tribal Lands', "MT Mesonet 8in Anomaly", "MT Mesonet 20in Anomaly"))
      
  } 
  saveWidget(m_raster, paste0(export.dir, "widgets/m_raster_", lower_variable[v], ".html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))
}
