library(tidyverse)
library(RCurl)
library(lubridate)

`%notin%` = Negate(`%in%`)

source('/home/zhoylman/mco-drought-indicators/processing/ancillary-functions/R/drought-functions.R')

data = list.files('/home/zhoylman/mesonet-download-data', full.names = T) %>%
  lapply(., read_csv) %>%
  data.table::rbindlist() %>%
  as_tibble()

gc()

stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv() %>%
  dplyr::select(`Station ID`, Longitude, Latitude, `Station name`) %>%
  rename(station_key = `Station ID`)

anom = data %>%
  mutate(hour = hour(datetime)) %>%
  filter(hour == 0,
         name %in% c("soilwc00", "soilwc04", "soilwc08", "soilwc20", "soilwc36"),
         station_key %notin% c('whitshaw', 'mdaglasw', 'blmroyno')) %>%
  mutate(week = lubridate::week(datetime)) %>%
  filter(week %in% c(lubridate::week(Sys.Date()),lubridate::week(Sys.Date())-1))%>%
  group_by(station_key, name) %>%
  mutate(anom = gamma_fit_spi(value, return_latest = F))%>%
  ungroup() %>%
  group_by(station_key) %>%
  filter(datetime == max(datetime)) %>%
  left_join(., stations, by = 'station_key')

gc()

write_csv(anom, '/home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/current_anom.csv')
