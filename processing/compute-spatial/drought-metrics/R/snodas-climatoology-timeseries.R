# this is to be run on zoran!!
library(tidyverse)
library(terra)
library(exactextractr)
library(sf)

source('/home/zhoylman/mco-drought-indicators/processing/ancillary-functions/R/drought-functions.R')

basins = read_sf('/home/zhoylman/mco-drought-indicators/processing/base-data/processed/watersheds_umrb.shp')

swe = list.files('/mnt/data1/snodas/processed/swe', full.names = T) %>%
  as_tibble() %>%
  mutate(time = fdates(value) %>% as.Date(., format = '%Y%m%d'))

out_data = matrix(nrow = length(swe$time),
                  ncol = length(basins$HUC8)) %>%
  as.data.frame()

colnames(out_data) = basins$HUC8

for(i in 1:length(swe$time)){
  print(i)
  temp_swe = rast(swe$value[i])
  
  temp_extraction = exact_extract(temp_swe, basins, fun = 'sum')
  
  out_data[i,] = temp_extraction 
}

out_data_final = out_data %>%
  as_tibble() %>%
  mutate(time = swe$time) %>%
  dplyr::select(time, everything())

write_csv(out_data_final, '/home/zhoylman/temp/snodas_watershed_sum_swe.csv')
