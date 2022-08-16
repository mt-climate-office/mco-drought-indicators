library(dplyr)
library(RCurl)
library(lubridate)
library(readr)
library(magrittr)
library(leaflet)
library(sf)
library(ggplot2)

`%notin%` = Negate(`%in%`)

source('/home/zhoylman/mco-drought-indicators/processing/ancillary-functions/R/drought-functions.R')

stations = getURL("https://mesonet.climate.umt.edu/api/v2/stations?type=csv&clean=true") %>%
  read_csv() %>%
  mutate(start_year = substr(date_installed, 1, 4) %>% as.numeric()) %>%
  #filter(start_year <= 2020) %>%
  dplyr::select(station, longitude, latitude, name, start_year) %>%
  rename(station_key = station)


data = list.files('/home/zhoylman/mesonet-download-data', full.names = T) %>%
  as_tibble() %>%
  filter(grepl(paste0(c(stations$station_key), collapse = '|'), value)) %$%
  value %>%
  lapply(., read_csv) %>%
  data.table::rbindlist() %>%
  as_tibble()

gc()

#compute days of interest (and adjust for day changes over a year or under)
today = Sys.Date()
#today = as.Date('2021-11-25')
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

trim_stations_cover_crop = c('conradmt', 'havrenmt')

anom = data %>%
  mutate(hour = hour(datetime),
         minute = minute(datetime),
         year = year(datetime)) %>%
  dplyr::filter(hour == 0,
         minute == 0,
         name %in% c("soilwc00", "soilwc04", "soilwc08", "soilwc20", "soilwc36"),
         station_key %notin% c('whitshaw', 'mdaglasw', 'blmroyno', 'mdachine')) %>%
  mutate(yday = lubridate::yday(datetime),
         value = ifelse(value > 1, value/100, value),
         value = ifelse(station_key %in% trim_stations_cover_crop & year < 2020, NA, value)) %>%
  dplyr::filter(yday %in% days_of_interest)%>%
  group_by(station_key, name) %>%
  tidyr::drop_na(value) %>%
  mutate(n = ifelse(name == 'soilwc04', sum(name == 'soilwc04'),
                    ifelse(name == 'soilwc08', sum(name == 'soilwc08'),
                           ifelse(name == 'soilwc20', sum(name == 'soilwc20'),
                                  ifelse(name == 'soilwc36', sum(name == 'soilwc36'),NA))))) %>%
  mutate(anom = gamma_fit_spi(value, return_latest = F, climatology_length = Inf))%>%
  ungroup() %>%
  group_by(station_key, name) %>%
  dplyr::filter(datetime == max(datetime)) %>%
  ungroup() %>%
  #filter for 4 years of data
  #dplyr::filter(n > 100)%>%
  mutate(anom = ifelse(n > 100, anom, NA)) %>%
  left_join(., stations, by = 'station_key')

gc()

write_csv(anom, '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/current_anom.csv')

## create geojsons for each depth

pal = colorBin(colorRamp(c("#730000", "#E60000", "#FFAA00", "#FCD37F", "#FFFF00", "#FFFFFF", '#82FCF9', '#32E1FA', '#325CFE', '#4030E3', '#303B83'), interpolate = "linear"), 
               domain = -2.5:2.5, bins = c(-Inf, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, Inf), na.color = "black")

anom_sf = st_as_sf(anom, coords = c('longitude', 'latitude')) %>%
  st_set_crs(st_crs('EPSG:4326')) %>%
  mutate(fillColor = pal(anom)) %>%
  #ensure data is at least within the last 4 days
  filter(datetime > Sys.time()-(86400*4))%>%
  mutate(name = name.x)

st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc04'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_04in.geojson', delete_dsn = T,append=FALSE)
st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc08'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_08in.geojson', delete_dsn = T,append=FALSE)
st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc20'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_20in.geojson', delete_dsn = T,append=FALSE)
st_write(anom_sf %>% dplyr::filter(name.x == 'soilwc36'), '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_36in.geojson', delete_dsn = T,append=FALSE)

# plot anoms

weeks_of_interest = c((Sys.time() %>% lubridate::week() - 12):  (Sys.time() %>% lubridate::week()))
weeks_of_interest = weeks_of_interest[weeks_of_interest > 0]

sm_data = data %>%
  dplyr::filter(name %in% c('soilwc04', 'soilwc08', 'soilwc20', 'soilwc36'),
                station_key %notin% c('whitshaw', 'mdaglasw', 'blmroyno', 'mdachine')) %>%
  mutate(year = lubridate::year(datetime),
         yday = lubridate::yday(datetime),
         month = lubridate::month(datetime),
         week = lubridate::week(datetime),
         value = ifelse(value > 1, value/100, value),
         value = ifelse(station_key %in% trim_stations_cover_crop & year < 2020, NA, value)) %>%
  dplyr::filter(week %in% weeks_of_interest) 

depths = c('soilwc04', 'soilwc08', 'soilwc20', 'soilwc36')
depths_lay = c('4in', '8in', '20in', '36in')

for(d in 1:length(depths)){
  temp_stations = anom_sf %>% dplyr::filter(name.x == depths[d]) %$% station_key %>% unique()
  for(i in 1:length(temp_stations)){
    tryCatch({
      grouped_cols = c('2017' = "#FF0000", '2018' = "#FFBF00", 
                       '2019' = "#00FF40", '2020' = '#00d9ff',
                       '2021' =  '#0040FF', '2022' = '#A020F0',
                       '2023' = '#000000')
      
      temp_data = sm_data %>%
        dplyr::filter(name == depths[d],
                      station_key == temp_stations[i]) %>%
        group_by(yday, year) %>%
        summarise(mean = median(value)) %>%
        mutate(time = as.Date("2022-01-01") + (yday-1))
      
      max_time =  sm_data %>%
        dplyr::filter(name == depths[d],
                      station_key == temp_stations[i]) %$%
        datetime %>%
        max() %>%
        as.Date()
      
      
      
      plot = ggplot()+
        geom_line(data = temp_data, aes(x = time, y = mean, color = year %>% as.factor), alpha = 0.2)+
        geom_point(data = temp_data, aes(x = time, y = mean, color = year %>% as.factor), alpha = 0.2, shape = 21)+
        geom_line(data = temp_data %>% dplyr::filter(year == Sys.time() %>% lubridate::year()), aes(x = time, y = mean, color = year %>% as.factor))+
        geom_point(data = temp_data %>% dplyr::filter(year == Sys.time() %>% lubridate::year()), aes(x = time, y = mean, color = year %>% as.factor), size = 0.5)+
        theme_bw(base_size = 12)+
        labs(y = 'Soil Moisture (m³/m³)', x = NULL)+
        theme(legend.position = 'bottom',
              legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5))+
        guides(colour = guide_legend(nrow = 1))+
        ggtitle(paste0(depths_lay[d], ' Soil Moisture Data (', max_time,')\nMT Mesonet: ', stations$name[which(stations$station_key == temp_stations[i])]))+
        scale_color_manual(values=grouped_cols)
      
      
      ggsave(plot, file = paste0('/home/zhoylman/mco-drought-indicators-data/mesonet/plots/', temp_stations[i], '_', depths[d], '_current.png'), height = 5, width = 5, units = 'in')
      
    }, error = function(e){
      NA
    })
  }
}
