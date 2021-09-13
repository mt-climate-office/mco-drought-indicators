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

states_i = c('WA', 'OR', 'MT', 'ID', 'WY', 'ND', 'SD')

for(s in 1:length(states_i)){
  usdm = st_read('/home/zhoylman/mco-drought-indicators-data/usdm/current_usdm.shp')%>%
    st_intersection(., states_sf %>% filter(STATE_ABBR == states_i[s]))
  
  county = sf::read_sf('/home/zhoylman/mco-drought-indicators/processing/base-data/processed/county_umrb.shp') %>%
    st_intersection(., states_sf %>% filter(STATE_ABBR == states_i[s]))
  
  for(m in 1:length(metric)){
    time = read_csv(paste0(base_url, metric[m], '/time.csv')) %$% time
    for(i in 1:length(time_scales)){
      drought = raster::raster(paste0("/vsicurl/https://data.climate.umt.edu/drought-indicators/", 
                                      metric[m], "/current_", metric[m], '_', time_scales[i], '.tif')) %>%
        raster::crop(., states %>% filter(STATE_ABBR == states_i[s])) %>%
        raster::mask(., states %>% filter(STATE_ABBR == states_i[s])) %>%
        raster::rasterToPoints() %>%
        as_tibble() %>%
        `colnames<-`(c('x','y','z')) %>%
        mutate(`Dx Class` = .bincode(z, breaks = rev(c(Inf, -0.5,-0.8,-1.3,-1.6,-2,-Inf))) %>% as.factor(),
               `Dx Class` = dplyr::recode_factor(`Dx Class`, `1` = paste0(name[m],' < -2 (D4)'), `2` = paste0('-2 < ',name[m],' < -1.6 (D3)'),
                                                 `3` = paste0('-1.6 < ',name[m],' < -1.3 (D2)'), `4` = paste0('-1.3 < ',name[m],' < -0.8 (D1)'),
                                                 `5` = paste0('-0.8 < ',name[m],' < -0.5 (D0)'), `6` = paste0(name[m],' > -0.5 (No Dx)')))
      
      #super messy, this is to make sure all catagories have levels for accurate plotting and legend builds. 
      dummy = drought[1:6,]
      dummy[1:6,] = NA
      dummy$`Dx Class` = c(paste0(name[m],' < -2 (D4)'),paste0('-2 < ',name[m],' < -1.6 (D3)'),
                           paste0('-1.6 < ',name[m],' < -1.3 (D2)'),paste0('-1.3 < ',name[m],' < -0.8 (D1)'),
                           paste0('-0.8 < ',name[m],' < -0.5 (D0)'),paste0(name[m],' > -0.5 (No Dx)')) %>% as.factor()
      
      drought = dummy %>%
        bind_rows(drought)
      
      drought$`Dx Class` = factor(drought$`Dx Class`, levels = c(paste0(name[m],' < -2 (D4)'),paste0('-2 < ',name[m],' < -1.6 (D3)'),
                                                                 paste0('-1.6 < ',name[m],' < -1.3 (D2)'),paste0('-1.3 < ',name[m],' < -0.8 (D1)'),
                                                                 paste0('-0.8 < ',name[m],' < -0.5 (D0)'),paste0(name[m],' > -0.5 (No Dx)')))
      
      map = ggplot() +
        geom_tile(data = drought, aes(x = x, y = y, fill = `Dx Class`))+
        ggtitle(paste0(time_scale_names[i], ' ', name[m],' (', time, ')'))+
        scale_fill_manual(values = rev(c("white","#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616")),
                          name = 'Produced by the Montana Climate Office (contact: zachary.hoylman@umontana.edu)', drop = F)+
        geom_sf(data = county, fill = 'transparent', color = 'black', size = 0.1)+
        theme_bw(base_size = 12)+ 
        labs(x = NULL, y = NULL)+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position="bottom",
              legend.text=element_text(size=10),
              legend.box.background = element_rect(colour = "black"),
              legend.title=element_text(size=8))+
        guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position = "bottom", title.hjust = 0.5))
      
      ggsave(map, file = paste0('/home/zhoylman/mco-drought-indicators-data/figures/usdm-color-scheme/', states_i[s],'/',
                                states_i[s], '_',metric[m], '_', time_scales[i], '.png'), width = 7, height = 5, units = 'in')
      # usdm_map = ggplot() +
      #   geom_tile(data = drought, aes(x = x, y = y, fill = `Dx Class`))+
      #   #geom_sf_text(data = usdm, aes( label = paste0('D',DM)), colour = 'black')+
      #   scale_fill_manual(values = rev(c("white","#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616")), name = NULL)+
      #   geom_sf(data = usdm, aes(fill = paste0('D',DM)))+
      #   scale_fill_manual(values = rev(c("#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616")), name = NULL)+
      #   #scale_colour_manual(values = (c("#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616")), name = NULL, guide = F)+
      #   ggtitle(paste0(time_scale_names[i], ' ', name[m],' (', time, ')'))+
      #   geom_sf(data = county, fill = 'transparent', color = 'black', size = 0.1)+
      #   theme_bw(base_size = 12)+ 
      #   labs(x = NULL, y = NULL)+
      #   theme(plot.title = element_text(hjust = 0.5),
      #         legend.position="bottom",
      #         legend.text=element_text(size=10),
      #         legend.box.background = element_rect(colour = "black"))
      # 
      # ggsave(usdm_map, file = paste0('/home/zhoylman/mco-drought-indicators-data/figures/usdm-color-scheme/usdm_',
      #                           metric[m], '_', time_scales[i], '.png'), width = 7, height = 4, units = 'in')
    }
  }
}


