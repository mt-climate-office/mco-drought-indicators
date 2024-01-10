library(dplyr)
library(RCurl)
library(spdplyr)
library(sf)
library(magrittr)
library(readr)
library(ggplot2)

states = rgdal::readOGR("/home/zhoylman/mco-drought-indicators/processing/base-data/raw/states.shp")
states_sf = sf::read_sf("/home/zhoylman/mco-drought-indicators/processing/base-data/raw/states.shp") 
  
name = c('SPEI')

states_i = c( 'MT')
m = 1
s = 1

#epsg:esri:102300

for(s in 1:length(states_i)){
  usdm = st_read('/home/zhoylman/mco-drought-indicators-data/usdm/current_usdm.shp')%>%
    st_intersection(., states_sf %>% filter(STATE_ABBR == states_i[s]))
  
  county = sf::read_sf('/home/zhoylman/mco-drought-indicators/processing/base-data/processed/county_umrb.shp') %>%
    st_intersection(., states_sf %>% filter(STATE_ABBR == states_i[s])) %>%
    st_transform(., 'ESRI:102300')
      time = 'June 1, 2021 - August 31, 2021'
   
       for(i in 1:length(time_scales)){
      drought = raster::raster('/home/zhoylman/temp/MT_drought_mngmt_spei_06-01_08-31.tif') %>%
        raster::crop(., states %>% filter(STATE_ABBR == states_i[s])) %>%
        raster::mask(., states %>% filter(STATE_ABBR == states_i[s])) %>%
        raster::projectRaster(., crs = crs('ESRI:102300')) %>%
        raster::rasterToPoints() %>%
        as_tibble() %>%
        `colnames<-`(c('x','y','z')) %>%
        mutate(`Dx Class` = .bincode(z, breaks = rev(c(Inf, -0.5,-0.8,-1.3,-1.6,-2,-Inf))) %>% as.factor(),
               `Dx Class` = dplyr::recode_factor(`Dx Class`, `1` = paste0(name[m],' ≤ -2 (D4)'), `2` = paste0('-2 < ',name[m],' ≤ -1.6 (D3)'),
                                                 `3` = paste0('-1.6 < ',name[m],' ≤ -1.3 (D2)'), `4` = paste0('-1.3 < ',name[m],' ≤ -0.8 (D1)'),
                                                 `5` = paste0('-0.8 < ',name[m],' ≤ -0.5 (D0)'), `6` = paste0(name[m],' > -0.5 (No Dx)')))
      
      #super messy, this is to make sure all catagories have levels for accurate plotting and legend builds. 
      dummy = drought[1:6,]
      dummy[1:6,] = NA
      dummy$`Dx Class` =  c(paste0(name[m],' ≤ -2 (D4)'),paste0('-2 < ',name[m],' ≤ -1.6 (D3)'),
                            paste0('-1.6 < ',name[m],' ≤ -1.3 (D2)'),paste0('-1.3 < ',name[m],' ≤ -0.8 (D1)'),
                            paste0('-0.8 < ',name[m],' ≤ -0.5 (D0)'),paste0(name[m],' > -0.5 (No Dx)')) %>% as.factor()
      
      drought = dummy %>%
        bind_rows(drought)
      
      drought$`Dx Class` = factor(drought$`Dx Class`, levels = c(paste0(name[m],' ≤ -2 (D4)'),paste0('-2 < ',name[m],' ≤ -1.6 (D3)'),
                                                                 paste0('-1.6 < ',name[m],' ≤ -1.3 (D2)'),paste0('-1.3 < ',name[m],' ≤ -0.8 (D1)'),
                                                                 paste0('-0.8 < ',name[m],' ≤ -0.5 (D0)'),paste0(name[m],' > -0.5 (No Dx)')))
      
      map = ggplot() +
        geom_tile(data = drought, aes(x = x, y = y, fill = `Dx Class`), color = 'black')+
        geom_tile(data = drought, aes(x = x, y = y, fill = `Dx Class`))+
        ggtitle(paste0('Standardized Precipitation Evapotranspiration Index (SPEI)',' \n', time))+
        scale_fill_manual(values = (c("white","#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616")),
                          breaks = rev(dummy$`Dx Class`) ,name = 'Montana Climate Office', drop = F)+
        geom_sf(data = county, fill = 'transparent', color = 'black', size = 0.3)+
        geom_sf(data = states_sf %>% filter(STATE_ABBR == states_i[s]) %>% st_transform(., 'ESRI:102300'), 
                fill = 'transparent', color = 'black', size = 1)+
        theme_void(base_size = 12)+ 
        labs(x = NULL, y = NULL)+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position="bottom",
              legend.text=element_text(size=12),
              legend.box.background = element_rect(colour = "black", size = 0.5),
              legend.title=element_text(size=10),
              legend.margin=margin(c(1, 1, 1, 1),unit="mm"))+
        scale_color_manual(values = 'black')+
        guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position = "bottom", title.hjust = 1))+
        theme(plot.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt"))
      
      ggsave(map, file = paste0('/home/zhoylman/temp/MT_drought_mngmt_spei_06-01_08-31.png'), width = 7, height = 5, units = 'in', dpi = 301)
      
    }
  
}


