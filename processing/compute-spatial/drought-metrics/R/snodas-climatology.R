# THIS ANALYSIS REQUIRES THE FULL SNODAS CLIMATOLOGY!
library(sf)
library(raster)
library(tidyverse)
library(lubridate)
library(terra)

source('/home/zhoylman/mco-drought-indicators/processing/ancillary-functions/R/drought-functions.R')

yday.waterYear = function(x, start.month = 10L){
  day = day(x)
  month = month(x)
  #dont want yday to go from 1 - 366, rather to 365
  new_date = make_date(2022, month, day)
  start.yr = year(new_date) - (month(new_date) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(new_date - start.date + 1L)
}

snodas_files = list.files('/media/zhoylman/ScratchDrive/snodas/processed/swe', full.names = T) %>%
  as_tibble() %>%
  mutate(date = fdates(value) %>% as.Date(., format = '%Y%m%d'),
         yday = yday(date),
         year = year(date),
         water_year_day = yday.waterYear(date))

######
watersheds = st_read('/home/zhoylman/mco-drought-indicators/processing/base-data/processed/watersheds_umrb.shp') 
names = watersheds$HUC8

i = 1

roi = watersheds %>% filter(HUC8 == names[i])

water_year_i = 90

swe = snodas_files %>%
  filter(water_year_day == water_year_i) %$%
  value %>%
  lapply(., terra::rast) %>%
  lapply(., terra::crop, roi %>% vect) %>%
  lapply(., terra::mask, roi %>% vect) %>%
  lapply(., terra::project, "epsg:5070")

swe = swe %>%
  lapply(., terra::resample, swe[[length(swe)]], method="bilinear") %>%
  rast 

tapp(swe, 1:nlyr(swe), fun = sum)
