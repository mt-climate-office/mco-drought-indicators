library(rgdal)
library(sf)
library(dplyr)
library(stringr)

states = read_sf("/home/zhoylman/mco-drought-indicators/processing/base-data/raw/states.shp") %>%
  st_transform(., 4326)

county = read_sf("/home/zhoylman/Downloads/US_County_Boundaries/US_County_Boundaries.shp")

#from https://www.hydroshare.org/resource/b832a6c2f96541808444ec9562c5247e/
watersheds = read_sf("/home/zhoylman/Downloads/huc8_conus.zip/HUC8_CONUS/HUC8_US.shp")

states_umrb = states %>% 
  dplyr::filter(STATE_NAME == "Montana"|STATE_NAME == "Idaho"| STATE_NAME == "Wyoming"|
                  STATE_NAME == "South Dakota"|STATE_NAME == "North Dakota"|
                  STATE_NAME == "Oregon"|STATE_NAME == "Washington")%>%
  sf::st_simplify() %>%
  st_transform(., 4326)

county_umrb = county %>%
  dplyr::filter(STATE == "Montana"|STATE == "Idaho"| STATE == "Wyoming"|
                  STATE == "South Dakota"|STATE == "North Dakota"|
                  STATE == "Oregon"|STATE == "Washington")%>%
  sf::st_simplify() %>%
  mutate(NAME = COUNTY)

#outline_umrb = st_combine(states_umrb) %>%
#  sf::st_simplify()

#st_crs(outline_umrb) = 4326

watersheds_umrb = watersheds %>%
  dplyr::filter(str_detect(STATES, 'MT') | str_detect(STATES, 'ID') | str_detect(STATES, 'WY') |
                str_detect(STATES, 'SD') | str_detect(STATES, 'ND') | str_detect(STATES, 'OR') |
                str_detect(STATES, 'WA')) %>%
  sf::st_intersection(., st_union(states))
  
watersheds_umrb_simple = rmapshaper::ms_simplify(watersheds_umrb, keep = 0.005)

outline_umrb = st_union(watersheds_umrb_simple)

county_umrb_simple =  rmapshaper::ms_simplify(county_umrb, keep = 0.02)


#this all needs to be updated!
st_write(county_umrb_simple, "/home/zhoylman/drought_indicators/shp_kml/larger_extent/county_umrb.shp", "county_umrb", driver = "ESRI Shapefile")
st_write(outline_umrb, "/home/zhoylman/drought_indicators/shp_kml/larger_extent/outline_umrb.shp", "outline_umrb", driver = "ESRI Shapefile")
st_write(watersheds_umrb_simple, "/home/zhoylman/mco-drought-indicators/processing/base-data/processed/watersheds_umrb.shp", "watersheds_umrb", driver = "ESRI Shapefile", append = F)



#clip tribal to domain. 
tribal = st_read('/home/zhoylman/drought_indicators/shp_kml/indlanp010g.gdb') %>%
  dplyr::filter(str_detect(STATE, 'MT') | str_detect(STATE, 'ID') | str_detect(STATE, 'WY') |
                  str_detect(STATE, 'SD') | str_detect(STATE, 'ND') | str_detect(STATE, 'OR') |
                  str_detect(STATE, 'WA') | str_detect(GNIS_Name1, 'Fort Belknap Reservation')) %>%
  st_transform(., 4326) %>%
  sf::st_simplify()

tribal_simple =  rmapshaper::ms_simplify(tribal, keep = 0.005)

st_write(tribal_simple, '/home/zhoylman/drought_indicators/shp_kml/larger_extent/UMRB_tribal_lands_simple.geojson')
