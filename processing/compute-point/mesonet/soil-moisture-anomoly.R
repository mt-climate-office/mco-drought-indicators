# Load required libraries
library(dplyr)
library(RCurl)
library(lubridate)
library(readr)
library(magrittr)
library(leaflet)
library(sf)
library(ggplot2)
library(tidyr)

# Define the number of observations required for anomaly calculation
min_years = 3
min_n = 31 * min_years

# Define the notin operator
`%notin%` = Negate(`%in%`)

# Source external functions from 'drought-functions.R'
source('/home/zhoylman/mco-drought-indicators/processing/ancillary-functions/R/drought-functions.R')

# Define API URL for retrieving daily climate observations
url = 'https://mesonet.climate.umt.edu/api/v2/observations/daily/?na_info=false&premade=true&latest=false&type=csv&rm_na=true&active=true&public=true&wide=true&units=us&tz=America%2FDenver&simple_datetime=true&agg_func=avg&start_time=2000-01-01T00%3A00%3A00&level=1&elements=soil_vwc,soil_temp&stations='

# Retrieve and read the raw response from the API
raw_response = getURL(url) %>%
  read_csv()

# Create a full dataset from the raw response
full_dataset = raw_response

# Compute days of interest for anomaly calculation
today = Sys.Date()-1
compute_days_of_interest = function(today){
  # Compute days of interest within a 31-day window
  today_yday = lubridate::yday(today)
  min_yday = lubridate::yday(today-15)
  max_yday =  lubridate::yday(today+15)
  
  if(today_yday + 15 > 366){
    max_day_seq = c((today_yday:366), (1:max_yday))
  } else {
    max_day_seq = today_yday:max_yday
  }
  
  if(today_yday - 15 < 1){
    min_day_seq = c(min_yday:366, 1:today_yday)
  } else {
    min_day_seq = min_yday:today_yday
  }
  days_of_interest = c(min_day_seq, max_day_seq) %>% unique()
  return(days_of_interest)
}
days_of_interest = compute_days_of_interest(today)

# Filter data based on stations and days of interest
data_filtered = full_dataset %>%
  filter(station %notin% c('whitshaw', 'mdaglasw', 'blmroyno', 'mdachine')) %>%
  mutate(yday = yday(datetime)) %>%
  filter(yday %in% days_of_interest) %>%
  select(-yday)

# Retrieve station information from the API
stations = getURL("https://mesonet.climate.umt.edu/api/v2/stations/?type=csv") %>%
  read_csv() %>%
  mutate(start_year = substr(date_installed, 1, 4) %>% as.numeric()) %>%
  dplyr::select(station, longitude, latitude, name, start_year) %>%
  filter(station %in% unique(data_filtered$station))

# Process and filter the data for generalized analysis
generalized_data_filtered = data_filtered %>% 
  select(station, datetime, contains('VWC')) %>%
  pivot_longer(cols = -c('station',  'datetime')) %>%
  mutate(depth = parse_number(name),
         frozen = ifelse(value < 32, T, F)) %>%
  mutate(generalized_depth = ifelse(depth < -51, 'Deep',
                                    ifelse(depth >-51 & depth < -10, 'Middle', 'Shallow')))%>%
  group_by(station, datetime, generalized_depth) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup()

# Define a function named frozen_fun for freezing condition check
frozen_fun = function(x) {
  # Function to check if elements are frozen
  tryCatch({
    if (any(x)) {
      return(TRUE)  # Return TRUE if any element is TRUE
    } else if (all(is.na(x))) {
      return(NA)    # Return NA if all elements are NA
    } else {
      return(FALSE)  # Return FALSE if there are both NA and FALSE
    } 
  }, error = function(e){
    return(NA)  # Return NA if an error occurs
  })
}

# Determine currently frozen conditions
currently_frozen = data_filtered %>%
  filter(datetime == as.POSIXct(today)) %>%
  select(station, datetime, contains('Temperature')) %>%
  pivot_longer(-c(station, datetime)) %>%
  mutate(depth = parse_number(name),
         generalized_depth = ifelse(depth < -51, 'Deep',
                                    ifelse(depth >-51 & depth < -10, 'Middle', 'Shallow')),
         frozen = ifelse(value < 32, T, F)) %>%
  group_by(station, generalized_depth) %>%
  summarise(frozen = frozen_fun(frozen),
            )

# Compute anomalies and join with other data
anom = generalized_data_filtered %>%
  drop_na() %>%
  group_by(station, generalized_depth) %>%
  mutate(anom = gamma_fit_spi(value, return_latest = F, climatology_length = Inf),
         n = length(value),
         anom = ifelse(n <= min_n, NA, anom)) %>%
  ungroup() %>%
  filter(datetime == as.POSIXct(today)) %>%
  select(-datetime) %>% 
  left_join(currently_frozen, ., by = c('station','generalized_depth')) %>%
  left_join(., stations, by = c('station')) %>%
  ungroup()

# Define the color scaling
pal = colorBin(colorRamp(c("#730000", "#E60000", "#FFAA00", "#FCD37F", "#FFFF00", "#FFFFFF", '#82FCF9', '#32E1FA', '#325CFE', '#4030E3', '#303B83'), interpolate = "linear"),
               domain = -2.5:2.5, bins = c(-Inf, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, Inf), na.color = "#000000")

# Convert to spatial data frame and write to GeoJSON files
anom_sf = st_as_sf(anom, coords = c('longitude', 'latitude')) %>%
  st_set_crs(st_crs('EPSG:4326')) %>%
  mutate(fillColor = pal(anom),
         fillColor = ifelse(frozen == T & (!is.na(frozen)), '#808080', fillColor))

st_write(anom_sf %>% dplyr::filter(generalized_depth == 'Shallow')%>% drop_na(value), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_shallow.geojson', delete_dsn = T,append=FALSE)
st_write(anom_sf %>% dplyr::filter(generalized_depth == 'Middle')%>% drop_na(value), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_middle.geojson', delete_dsn = T,append=FALSE)
st_write(anom_sf %>% dplyr::filter(generalized_depth == 'Deep') %>% drop_na(value), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_deep.geojson', delete_dsn = T,append=FALSE)

#plot conditions

weeks_of_interest = c((Sys.time() %>% lubridate::week() - 12):  (Sys.time() %>% lubridate::week()))
year_start = lubridate::year(Sys.time())
corrected_weeks = c()

for(i in 1: length(weeks_of_interest)){
  if(i == 1){
    if(any(weeks_of_interest < 0)){
      year_start = year_start-1
    }
  }
  if(weeks_of_interest[i] < 1){
    weeks_of_interest[i] = weeks_of_interest[i] + 52
    corrected_weeks[i] = weeks_of_interest[i]
  }
}

plotting_data = full_dataset %>%
  select(station, datetime, contains('VWC')) %>%
  pivot_longer(cols = -c('station',  'datetime')) %>%
  mutate(week = lubridate::week(datetime)) %>%
  filter(week %in% weeks_of_interest) %>%
  mutate(depth = parse_number(name),
         frozen = ifelse(value < 32, T, F)) %>%
  mutate(generalized_depth = ifelse(depth < -51, 'Deep',
                                    ifelse(depth >-51 & depth < -10, 'Middle', 'Shallow')))%>%
  group_by(station, datetime, generalized_depth) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(week = lubridate::week(datetime),
         year_org = lubridate::year(datetime),
         yday = lubridate::yday(datetime),
         month = lubridate::month(datetime),
         year = ifelse(week %in% corrected_weeks, year_org+1, year_org),
         plot_year = year)

if(year_start != lubridate::year(Sys.Date())){
  plotting_data = plotting_data %>%
    mutate(plot_year = paste0(year-1, '/', year))
}

starting_date = lubridate::make_date(year_start, month = 1, day = 1) + lubridate::weeks(weeks_of_interest[1] - 1) - 1


depths = c('Shallow', 'Middle', 'Deep')
depths_lay = c('Shallow (0-4in)', 'Mid-Depth (8-20in)', 'Deep (28-40in)')

for(d in 1:length(depths)){
  temp_stations = anom_sf %>% dplyr::filter(generalized_depth == depths[d]) %$% station %>% unique()
  for(i in 1:length(temp_stations)){
    tryCatch({
      grouped_cols = c('2017' = "#006302", '2018' = "#FFBF00",
                       '2019' = "#00FF40", '2020' = '#00d9ff',
                       '2021' =  '#0040FF', '2022' = '#A020F0',
                       '2023' = '#FF0000', '2024' = '#000000')
      
      if(year_start != lubridate::year(Sys.Date())){
        grouped_cols =  c('2016/2017' = '#006302', 
                          '2017/2018' = "#FF0000", '2018/2019' = "#FFBF00",
                          '2019/2020' = "#00FF40", '2020/2021' = '#00d9ff',
                          '2021/2022' =  '#0040FF', '2022/2023' = '#A020F0',
                          '2023/2024' = '#000000')
      }

      temp_data = plotting_data %>%
        dplyr::filter(generalized_depth == depths[d],
                      station == temp_stations[i]) %>%
        mutate(year_offset = abs(year - 2024), 
              time = ifelse(year == 2024, as.Date(datetime), as.Date('2023-01-01') + (yday-1)),
              time = ifelse(year_offset > 0 , as.Date(time) + lubridate::years(1), as.Date(time)),
              time = as.Date(time))

      max_time =  plotting_data %>%
        dplyr::filter(generalized_depth == depths[d],
                      station == temp_stations[i]) %$%
        datetime %>%
        max() %>%
        as.Date() %>%
        format(., "%m-%d-%Y")

      plot = ggplot()+
        geom_line(data = temp_data, aes(x = time, y = value, color = plot_year %>% as.factor), alpha = 0.2)+
        geom_point(data = temp_data, aes(x = time, y = value, color = plot_year %>% as.factor), alpha = 0.2, shape = 21)+
        geom_line(data = temp_data %>% dplyr::filter(year == Sys.time() %>% lubridate::year()), aes(x = time, y = value, color = plot_year %>% as.factor))+
        geom_point(data = temp_data %>% dplyr::filter(year == Sys.time() %>% lubridate::year()), aes(x = time, y = value, color = plot_year %>% as.factor), size = 0.5)+
        theme_bw(base_size = 14)+
        labs(y = 'Soil Moisture (m³/m³)', x = NULL)+
        theme(legend.position = 'bottom',
              legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5))+
        guides(colour = guide_legend(ncol = 4))+
        ggtitle(paste0(depths_lay[d], ' Soil Moisture Data (', max_time,')\nMT Mesonet: ', stations$name[which(stations$station == temp_stations[i])]))+
        scale_color_manual(values=grouped_cols)


      ggsave(plot, file = paste0('/home/zhoylman/mco-drought-indicators-data/mesonet/plots/', temp_stations[i], '_', depths[d], '_current.png'), height = 5, width = 6, units = 'in')

    }, error = function(e){
      NA
    })
  }
}

  # anom = data %>%
  #   mutate(hour = hour(datetime),
  #          minute = minute(datetime),
  #          year = year(datetime)) %>%
  #   dplyr::filter(hour == 0,
  #          minute == 0,
  #          name %in% c("soilwc00", "soilwc04", "soilwc08", "soilwc20", "soilwc36"),
  #          station_key %notin% c('whitshaw', 'mdaglasw', 'blmroyno', 'mdachine')) %>%
  #   mutate(yday = lubridate::yday(datetime),
  #          value = ifelse(value > 1, value/100, value),
#          value = ifelse(station_key %in% trim_stations_cover_crop & year < 2020, NA, value)) %>%
#   dplyr::filter(yday %in% days_of_interest)%>%
#   group_by(station_key, name) %>%
#   tidyr::drop_na(value) %>%
#   mutate(n = ifelse(name == 'soilwc04', sum(name == 'soilwc04'),
#                     ifelse(name == 'soilwc08', sum(name == 'soilwc08'),
#                            ifelse(name == 'soilwc20', sum(name == 'soilwc20'),
#                                   ifelse(name == 'soilwc36', sum(name == 'soilwc36'),NA))))) %>%
#   mutate(anom = gamma_fit_spi(value, return_latest = F, climatology_length = Inf))%>%
#   ungroup() %>%
#   group_by(station_key, name) %>%
#   dplyr::filter(datetime == max(datetime)) %>%
#   ungroup() %>%
#   #filter for 4 years of data
#   #dplyr::filter(n > 100)%>%
#   mutate(anom = ifelse(n > 100, anom, NA)) %>%
#   left_join(., stations, by = 'station_key')
# 
  
  
  
##### Old Code ####
# data = list.files('/home/zhoylman/mesonet-download-data', full.names = T) %>%
#   as_tibble() %>%
#   filter(grepl(paste0(c(stations$station_key), collapse = '|'), value)) %$%
#   value %>%
#   lapply(., read_csv) %>%
#   data.table::rbindlist() %>%
#   as_tibble()
# 
# gc()
# 
# #compute days of interest (and adjust for day changes over a year or under)
# today = Sys.Date()
# #today = as.Date('2021-11-25')
# today_yday = lubridate::yday(today)
# min_yday = lubridate::yday(today-15)
# max_yday =  lubridate::yday(today+15)
# 
# if(today_yday + 15 > 366){
#   max_day_seq = c((today_yday:366), (1:max_yday))
# } else {
#   max_day_seq = today_yday:max_yday
# }
# 
# if(today_yday - 15 < 1){
#   min_day_seq = c(min_yday:366, 1:today_yday)
# } else {
#   min_day_seq = min_yday:today_yday
# }
# days_of_interest = c(min_day_seq, max_day_seq) %>% unique()
# 
# trim_stations_cover_crop = c('conradmt', 'havrenmt')
# 
# anom = data %>%
#   mutate(hour = hour(datetime),
#          minute = minute(datetime),
#          year = year(datetime)) %>%
#   dplyr::filter(hour == 0,
#          minute == 0,
#          name %in% c("soilwc00", "soilwc04", "soilwc08", "soilwc20", "soilwc36"),
#          station_key %notin% c('whitshaw', 'mdaglasw', 'blmroyno', 'mdachine')) %>%
#   mutate(yday = lubridate::yday(datetime),
#          value = ifelse(value > 1, value/100, value),
#          value = ifelse(station_key %in% trim_stations_cover_crop & year < 2020, NA, value)) %>%
#   dplyr::filter(yday %in% days_of_interest)%>%
#   group_by(station_key, name) %>%
#   tidyr::drop_na(value) %>%
#   mutate(n = ifelse(name == 'soilwc04', sum(name == 'soilwc04'),
#                     ifelse(name == 'soilwc08', sum(name == 'soilwc08'),
#                            ifelse(name == 'soilwc20', sum(name == 'soilwc20'),
#                                   ifelse(name == 'soilwc36', sum(name == 'soilwc36'),NA))))) %>%
#   mutate(anom = gamma_fit_spi(value, return_latest = F, climatology_length = Inf))%>%
#   ungroup() %>%
#   group_by(station_key, name) %>%
#   dplyr::filter(datetime == max(datetime)) %>%
#   ungroup() %>%
#   #filter for 4 years of data
#   #dplyr::filter(n > 100)%>%
#   mutate(anom = ifelse(n > 100, anom, NA)) %>%
#   left_join(., stations, by = 'station_key')
# 
# gc()
# 
# write_csv(anom, '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/current_anom.csv')
# 
# ## create geojsons for each depth
# 
# pal = colorBin(colorRamp(c("#730000", "#E60000", "#FFAA00", "#FCD37F", "#FFFF00", "#FFFFFF", '#82FCF9', '#32E1FA', '#325CFE', '#4030E3', '#303B83'), interpolate = "linear"), 
#                domain = -2.5:2.5, bins = c(-Inf, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, Inf), na.color = "black")
# 
# anom_sf = st_as_sf(anom, coords = c('longitude', 'latitude')) %>%
#   st_set_crs(st_crs('EPSG:4326')) %>%
#   mutate(fillColor = pal(anom)) %>%
#   #ensure data is at least within the last 4 days
#   filter(datetime > Sys.time()-(86400*4))%>%
#   mutate(name = name.x)
# 
# st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc04'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_04in.geojson', delete_dsn = T,append=FALSE)
# st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc08'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_08in.geojson', delete_dsn = T,append=FALSE)
# st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc20'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_20in.geojson', delete_dsn = T,append=FALSE)
# st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc36'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_36in.geojson', delete_dsn = T,append=FALSE)
# 
# # plot anoms
# 
# weeks_of_interest = c((Sys.time() %>% lubridate::week() - 12):  (Sys.time() %>% lubridate::week()))
# weeks_of_interest = weeks_of_interest[weeks_of_interest > 0]
# 
# sm_data = data %>%
#   dplyr::filter(name %in% c('soilwc04', 'soilwc08', 'soilwc20', 'soilwc36'),
#                 station_key %notin% c('whitshaw', 'mdaglasw', 'blmroyno', 'mdachine')) %>%
#   mutate(year = lubridate::year(datetime),
#          yday = lubridate::yday(datetime),
#          month = lubridate::month(datetime),
#          week = lubridate::week(datetime),
#          value = ifelse(value > 1, value/100, value),
#          value = ifelse(station_key %in% trim_stations_cover_crop & year < 2020, NA, value)) %>%
#   dplyr::filter(week %in% weeks_of_interest) 
# 
# depths = c('soilwc04', 'soilwc08', 'soilwc20', 'soilwc36')
# depths_lay = c('4in', '8in', '20in', '36in')
# 
# for(d in 1:length(depths)){
#   temp_stations = anom_sf %>% dplyr::filter(name.x == depths[d]) %$% station_key %>% unique()
#   for(i in 1:length(temp_stations)){
#     tryCatch({
#       grouped_cols = c('2017' = "#FF0000", '2018' = "#FFBF00", 
#                        '2019' = "#00FF40", '2020' = '#00d9ff',
#                        '2021' =  '#0040FF', '2022' = '#A020F0',
#                        '2023' = '#FF0000', '2024' = '#000000')
#       
#       temp_data = sm_data %>%
#         dplyr::filter(name == depths[d],
#                       station_key == temp_stations[i]) %>%
#         group_by(yday, year) %>%
#         summarise(mean = median(value)) %>%
#         mutate(time = as.Date("2023-01-01") + (yday-1))
#       
#       max_time =  sm_data %>%
#         dplyr::filter(name == depths[d],
#                       station_key == temp_stations[i]) %$%
#         datetime %>%
#         max() %>%
#         as.Date()
#       
#       
#       
#       plot = ggplot()+
#         geom_line(data = temp_data, aes(x = time, y = mean, color = year %>% as.factor), alpha = 0.2)+
#         geom_point(data = temp_data, aes(x = time, y = mean, color = year %>% as.factor), alpha = 0.2, shape = 21)+
#         geom_line(data = temp_data %>% dplyr::filter(year == Sys.time() %>% lubridate::year()), aes(x = time, y = mean, color = year %>% as.factor))+
#         geom_point(data = temp_data %>% dplyr::filter(year == Sys.time() %>% lubridate::year()), aes(x = time, y = mean, color = year %>% as.factor), size = 0.5)+
#         theme_bw(base_size = 12)+
#         labs(y = 'Soil Moisture (m³/m³)', x = NULL)+
#         theme(legend.position = 'bottom',
#               legend.title=element_blank(),
#               plot.title = element_text(hjust = 0.5))+
#         guides(colour = guide_legend(nrow = 2))+
#         ggtitle(paste0(depths_lay[d], ' Soil Moisture Data (', max_time,')\nMT Mesonet: ', stations$name[which(stations$station_key == temp_stations[i])]))+
#         scale_color_manual(values=grouped_cols)
#       
#       
#       ggsave(plot, file = paste0('/home/zhoylman/mco-drought-indicators-data/mesonet/plots/', temp_stations[i], '_', depths[d], '_current.png'), height = 5, width = 5, units = 'in')
#       
#     }, error = function(e){
#       NA
#     })
#   }
# }
