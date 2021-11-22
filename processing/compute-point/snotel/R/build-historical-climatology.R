library(dplyr)
library(RCurl)
library(snotelr)
library(lubridate)

yday.waterYear = function(x, start.month = 10L){
  day = day(x)
  month = month(x)
  #dont want yday to go from 1 - 366, rather to 365
  new_date = make_date(2022, month, day)
  start.yr = year(new_date) - (month(new_date) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(new_date - start.date + 1L)
}

sites = snotel_info()

filtered_sites = sites %>%
  filter(state %in% c('MT', 'ID', 'OR', 'WA', 'WY', 'ND', 'SD'),
         start <= as.Date('2000-10-01'))

start_year_id = filtered_sites %>%
  mutate(year = lubridate::year(start),
         year = ifelse(year < 1991, 1991, year))

readr::write_csv(start_year_id, '/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_meta.csv')

tictoc::tic()
data = snotel_download(site_id = filtered_sites$site_id, internal = TRUE) %>%
  select(site_id, date, snow_water_equivalent, precipitation_cumulative) %>%
  as_tibble()
tictoc::toc()

climatology = data %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>%
  slice(c(which(day != 29 & month != 2))) %>%
  filter(year >= 1991 & year <= 2020) %>%
  mutate(water_year_yday = yday.waterYear(date %>% as.Date())) %>%
  group_by(site_id, water_year_yday, month, day) %>%
  summarise(swe_q50 = quantile(snow_water_equivalent, 0.5, na.rm = T),
            swe_q75 = quantile(snow_water_equivalent, 0.75, na.rm = T),
            swe_q25 = quantile(snow_water_equivalent, 0.25, na.rm = T),
            swe_q05 = quantile(snow_water_equivalent, 0.05, na.rm = T),
            swe_q95 = quantile(snow_water_equivalent, 0.95, na.rm = T),
            swe_min = min(snow_water_equivalent, na.rm = T),
            swe_max = max(snow_water_equivalent, na.rm = T),
            precip_q50 = quantile(precipitation_cumulative, 0.5, na.rm = T),
            precip_q75 = quantile(precipitation_cumulative, 0.75, na.rm = T),
            precip_q25 = quantile(precipitation_cumulative, 0.25, na.rm = T),
            precip_q05 = quantile(precipitation_cumulative, 0.05, na.rm = T),
            precip_q95 = quantile(precipitation_cumulative, 0.95, na.rm = T),
            precip_min = min(precipitation_cumulative, na.rm = T),
            precip_max = max(precipitation_cumulative, na.rm = T)) %>%
  ungroup() 

readr::write_csv(climatology, '/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_climatology.csv')
