library(dplyr)
library(RCurl)
library(spdplyr)
library(sf)
library(magrittr)
library(readr)
library(ggplot2)

time_scales = c(15,30,60,90,180,365, 'water_year', 'year_to_date')
time_scale_names = c('15 Day','30 Day','60 Day','90 Day','180 Day','365 Day', 'Water Year', 'Year to Date')
metric = c('spi', 'spei')
name = c('SPI', 'SPEI')

base_url = 'https://data.climate.umt.edu/drought-indicators/'

states = rgdal::readOGR("/home/zhoylman/mco-drought-indicators/processing/base-data/raw/states.shp")
states_sf = sf::read_sf("/home/zhoylman/mco-drought-indicators/processing/base-data/raw/states.shp")

county = sf::read_sf('/home/zhoylman/mco-drought-indicators/processing/base-data/processed/county_umrb.shp') %>%
  st_intersection(., states_sf %>% filter(STATE_ABBR == 'MT'))

for(m in 1:length(metric)){
  time = read_csv(paste0(base_url, metric[m], '/time.csv')) %$% time
  for(i in 1:length(time_scales)){
    drought = raster::raster(paste0("/vsicurl/https://data.climate.umt.edu/drought-indicators/", 
                                    metric[m], "/current_", metric[m], '_', time_scales[i], '.tif')) %>%
      raster::crop(., states %>% filter(STATE_ABBR == 'MT')) %>%
      raster::mask(., states %>% filter(STATE_ABBR == 'MT')) %>%
      raster::rasterToPoints() %>%
      as_tibble() %>%
      `colnames<-`(c('x','y','z')) %>%
      mutate(`Dx Class` = .bincode(z, breaks = rev(c(-0.5,-0.8,-1.3,-1.6,-2,-Inf))) %>% as.factor(),
             `Dx Class` = dplyr::recode_factor(`Dx Class`, `1` = 'D4', `2` = 'D3',
                                               `3` = 'D2', `4` = 'D1', `5` = 'D0'))
    
    map = ggplot() +
      geom_tile(data = drought, aes(x = x, y = y, fill = `Dx Class`))+
      geom_sf(data = county, fill = 'transparent', color = 'black', size = 0.1)+
      scale_fill_manual(values = rev(c("#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616")))+
      ggtitle(paste0(time_scale_names[i], ' ', name[m],' (', time, ')'))+
      theme_bw(base_size = 16)+ 
      labs(x = NULL, y = NULL)+
      theme(plot.title = element_text(hjust = 0.5))
    
    map
    
    ggsave(map, file = paste0('/home/zhoylman/mco-drought-indicators-data/figures/usdm-color-scheme/',
                              metric[m], '_', time_scales[i], '.png'), width = 7, height = 4, units = 'in')
  }
}

