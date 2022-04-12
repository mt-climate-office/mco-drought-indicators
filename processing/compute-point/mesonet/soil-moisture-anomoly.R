library(dplyr)
library(RCurl)
library(lubridate)
library(readr)
library(magrittr)

`%notin%` = Negate(`%in%`)

source('/home/zhoylman/mco-drought-indicators/processing/ancillary-functions/R/drought-functions.R')

stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv() %>%
  mutate(start_year = substr(`Start date`, 9, 12) %>% as.numeric()) %>%
  filter(start_year <= 2018) %>%
  dplyr::select(`Station ID`, Longitude, Latitude, `Station name`) %>%
  rename(station_key = `Station ID`)


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

anom = data %>%
  mutate(hour = hour(datetime),
         minute = minute(datetime)) %>%
  filter(hour == 0,
         minute == 0,
         name %in% c("soilwc00", "soilwc04", "soilwc08", "soilwc20", "soilwc36"),
         station_key %notin% c('whitshaw', 'mdaglasw', 'blmroyno')) %>%
  mutate(yday = lubridate::yday(datetime)) %>%
  filter(yday %in% days_of_interest)%>%
  group_by(station_key, name) %>%
  mutate(anom = gamma_fit_spi(value, return_latest = F))%>%
  ungroup() %>%
  group_by(station_key) %>%
  filter(datetime == max(datetime)) %>%
  left_join(., stations, by = 'station_key')

gc()

write_csv(anom, '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/current_anom.csv')
