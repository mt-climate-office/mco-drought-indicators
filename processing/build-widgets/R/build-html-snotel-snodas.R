## This system is less refined. There are lots of custom differences between these maps. Need to 
## think about this a bit more, but want to get it opperational now. (2-21-2021)

#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#load all libraries for all apps
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#load custom functions
source(paste0(git.dir, '/processing/ancillary-functions/R/widget-functions.R'))
source(paste0(git.dir, '/processing/ancillary-functions/R/base-map.R'))

#import counties
counties = st_read(paste0(git.dir, 'processing/base-data/processed/county_umrb.shp'))
watersheds = st_read(paste0(git.dir, 'processing/base-data/processed/watersheds_umrb.shp'))
tribal = st_read(paste0(git.dir, 'processing/base-data/processed/UMRB_tribal_lands_simple.geojson'))

#import raw swe and depth 
current_swe = list.files('/home/zhoylman/mco-drought-indicators-data/snodas/processed/swe/', full.names = T) %>%
  as_tibble() %>%
  mutate(time = gsub("\\D", "",  value),
         time = as.Date(time, format = '%Y%m%d')) %>%
  filter(time == max(time)) %$%
  value %>%
  raster() %>%
  crop(., st_union(watersheds) %>% as(., 'Spatial')) %>%
  mask(., st_union(watersheds) %>% as(., 'Spatial'))

current_swe_time = list.files('/home/zhoylman/mco-drought-indicators-data/snodas/processed/swe/', full.names = T) %>%
  as_tibble() %>%
  mutate(time = gsub("\\D", "",  value),
         time = as.Date(time, format = '%Y%m%d')) %>%
  filter(time == max(time)) %$%
  time

current_depth = list.files('/home/zhoylman/mco-drought-indicators-data/snodas/processed/snow_depth/', full.names = T) %>%
  as_tibble() %>%
  mutate(time = gsub("\\D", "",  value),
         time = as.Date(time, format = '%Y%m%d')) %>%
  filter(time == max(time)) %$%
  value %>%
  raster() %>%
  crop(., st_union(watersheds) %>% as(., 'Spatial')) %>%
  mask(., st_union(watersheds) %>% as(., 'Spatial'))

current_depth_time = list.files('/home/zhoylman/mco-drought-indicators-data/snodas/processed/snow_depth/', full.names = T) %>%
  as_tibble() %>%
  mutate(time = gsub("\\D", "",  value),
         time = as.Date(time, format = '%Y%m%d')) %>%
  filter(time == max(time)) %$%
  time 

#crop range to 99th percentile and convert to integer
values(current_depth) = ifelse(values(current_depth) > quantile(values(current_depth), 0.999, na.rm = T),
                              quantile(values(current_depth), 0.999, na.rm = T),
                              values(current_depth))/25.4
values(current_depth) = as.integer(values(current_depth))
values(current_depth) = ifelse(values(current_depth) == 0, NA, values(current_depth))

values(current_swe) = ifelse(values(current_swe) > quantile(values(current_swe), 0.999, na.rm = T),
                               quantile(values(current_swe), 0.999, na.rm = T),
                               values(current_swe))/25.4
#values(current_swe) = as.integer(values(current_swe))
current_swe = current_swe * (current_swe > 0)
values(current_swe) = ifelse(values(current_swe) == 0, NA, values(current_swe))

#define input shp files
sites_of_interest = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_climatology.csv')

#load current conditions 
anom = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/anomaly/current_anomaly.csv') %>%
  mutate(swe_anom = ifelse(swe_anom > 150, 149, swe_anom),
         swe_anom = ifelse(swe_anom <= 50, 51, swe_anom),
         precip_anom = ifelse(precip_anom > 150, 149, precip_anom))

snotel = snotel_info() %>%
  dplyr::filter(site_id %in% unique(sites_of_interest$site_id)) %>%
  left_join(., anom, by = 'site_id') 

states =  st_read(paste0(git.dir, "processing/base-data/raw/states.shp"))

#Standardized swe import grid
snodas_standardized_swe = raster(paste0(export.dir, 'snodas/processed/standardized_swe/current_snodas_swe_standardized.tif'))

snodas_standardized_swe[snodas_standardized_swe >= 2.5] = 2.49
snodas_standardized_swe[snodas_standardized_swe <= -2.5] = -2.49

#color pallet
pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(snotel$swe_anom, na.rm = T),max(snotel$swe_anom, na.rm = T)), na.color = "grey")
pal_standard <- colorNumeric(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), -2.5:2.5, na.color = "transparent")
pal_standard_r <- colorNumeric(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), -2.5:2.5, na.color = "transparent")

pal_swe_raw = colorNumeric(c("cyan", "lightblue", "blue", "darkblue", "purple"), domain = c(min(values(current_swe), na.rm = T)-1,max(values(current_swe), na.rm = T)+1), na.color = "transparent")
pal_swe_raw_rev = colorNumeric(rev(c("cyan", "lightblue", "blue", "darkblue", "purple")), domain = c(min(values(current_swe), na.rm = T)-1,max(values(current_swe), na.rm = T)+1), na.color = "grey")
pal_depth_raw = colorNumeric(c("cyan", "lightblue", "blue", "darkblue", "purple"), domain = c(min(values(current_depth), na.rm = T)-1,max(values(current_depth), na.rm = T)+1), na.color = "transparent")
pal_depth_raw_rev = colorNumeric(rev(c("cyan", "lightblue", "blue", "darkblue", "purple")), domain = c(min(values(current_depth), na.rm = T)-1,max(values(current_depth), na.rm = T)+1), na.color = "grey")

#time id
snotel_time = snotel$Date %>% max()
snodas_time = read_csv('/home/zhoylman/mco-drought-indicators-data/snodas/processed/standardized_swe/time.csv')
#import snow depth change 
current_1 = raster::raster(paste0(export.dir, "snodas/processed/delta_snow_depth/delta_1_depth_in.tif"))
current_3 = raster::raster(paste0(export.dir, "snodas/processed/delta_snow_depth/delta_3_depth_in.tif"))
current_7 = raster::raster(paste0(export.dir, "snodas/processed/delta_snow_depth/delta_7_depth_in.tif"))

current_1[current_1 > 19.9] = 19.9
current_1[current_1 < -19.9] = -19.9

current_3[current_3 > 19.9] = 19.9
current_3[current_3 < -19.9] = -19.9

current_7[current_7 > 19.9] = 19.9
current_7[current_7 < -19.9] = -19.9

pal_r <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                  domain = -20:20, bins = c(-20,-10,-5,-3,-1,-0.5,0.5,1,3,5,10,20), na.color = "transparent")

pal_r_rev <- colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                      domain = -20:20, bins = c(-20,-10,-5,-3,-1,-0.5,0.5,1,3,5,10,20), na.color = "transparent")

pal <- colorNumeric(c('red', 'white' , 'blue'), domain = c(50,150), na.color = "grey")
pal_rev <- colorNumeric(rev(c('red', 'white' , 'blue')), domain = c(50,150), na.color = "grey")

pal_r_rev_numeric <- colorNumeric(colorRamp((c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"))), 
                      domain = -20:20, na.color = "transparent")

#custom legend fix
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

swe_map = base_map() %>%
  leaflet::addMapPane("SNOTEL (SWE)", zIndex = 420) %>%
  leaflet::addMapPane("Counties", zIndex = 410) %>%
  leaflet::addMapPane("Tribal Lands", zIndex = 410) %>%
  leaflet::addMapPane("Watersheds", zIndex = 410) %>%
  leaflet::addMapPane("USDM", zIndex = 400) %>%
  addCircleMarkers(snotel$longitude, snotel$latitude, snotel$sit_id,
                   popup = paste0("<img src='https://data.climate.umt.edu/drought-indicators/plots/snotel_plot_",
                                  snotel$site_id,".png' height='500' width='642' loading='lazy'/>",
                                  '<a  target="_blank" rel="noopener noreferrer" href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/daily/',
                                  snotel$site_id,':',snotel$state,':SNTL/-7,0/WTEQ::value,SNWD::value,PREC::value,TOBS::value,TMAX::value,TMIN::value,TAVG::value">Current Daily SNOTEL Table (last 7 Days)</a><br>',
                                  '<a  target="_blank" rel="noopener noreferrer" href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/hourly/',
                                  snotel$site_id,':',snotel$state,':SNTL/',Sys.Date(),',',Sys.Date(),'/WTEQ::value,SNWD::value,PREC::value,TOBS::value?sortBy=0:1">Current Hourly SNOTEL Table (Today)</a>'),
                   popupOptions = popupOptions(maxWidth ="auto", closeOnClick = TRUE),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(snotel$swe_anom), group = "SNOTEL (SWE)", options = popupOptions(maxWidth = 650) )%>%
  addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = watersheds, group = "Watersheds", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~NAME, labelOptions = labelOptions(textsize = '14px'))%>%
  addRasterImage(current_1, colors = pal_r, opacity = 0.8, group = "24hr Change", project = TRUE)%>%
  addRasterImage(current_3, colors = pal_r, opacity = 0.8, group = "72hr Change", project = TRUE)%>%
  addRasterImage(current_7, colors = pal_r, opacity = 0.8, group = "7 Day Change", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("24hr Change","72hr Change", "7 Day Change"),
                            overlayGroups = c("SNOTEL (SWE)", "States",  "Weather", "USDM","Counties", 'Watersheds', 'Tribal Lands'),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal_rev, values = 50:150,
            title = paste0("SNOTEL<br>% Average SWE<br>", snotel_time),
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend("bottomright", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",snodas_time$time),
            opacity = 1,
            group = "24hr Snow Change",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  leaflet::hideGroup(c("Watersheds", "Counties", 'Tribal Lands'))%>%
  setView(lng = -108, lat = 46.5, zoom = 6) %>%
  prependContent(tags$style(type = "text/css", css_fix))

saveWidget(swe_map, paste0(export.dir, "widgets/swe_snotel.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))

#standardized swe
standardized_swe_map = base_map() %>%
  leaflet::addMapPane("SNOTEL (SWE)", zIndex = 420) %>%
  leaflet::addMapPane("Counties", zIndex = 410) %>%
  leaflet::addMapPane("Tribal Lands", zIndex = 410) %>%
  leaflet::addMapPane("Hypsome-SWE", zIndex = 410) %>%
  leaflet::addMapPane("USDM", zIndex = 400) %>%
  addCircleMarkers(snotel$longitude, snotel$latitude, snotel$sit_id, 
                   popup = paste0("<img src='https://data.climate.umt.edu/drought-indicators/plots/snotel_plot_",
                                  snotel$site_id,".png' height='400' width='512' loading='lazy'/>", 
                                  '<a target="_blank" rel="noopener noreferrer" href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/daily/',
                                  snotel$site_id,':',snotel$state,':SNTL/-7,0/WTEQ::value,SNWD::value,PREC::value,TOBS::value,TMAX::value,TMIN::value,TAVG::value">Current Daily SNOTEL Table (last 7 Days)</a><br>',
                                  '<a target="_blank" rel="noopener noreferrer" href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/hourly/',
                                  snotel$site_id,':',snotel$state,':SNTL/',Sys.Date(),',',Sys.Date(),'/WTEQ::value,SNWD::value,PREC::value,TOBS::value?sortBy=0:1">Current Hourly SNOTEL Table (Today)</a>'),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(snotel$swe_anom), group = "SNOTEL (SWE)", popupOptions = popupOptions(maxWidth ="auto", closeOnClick = TRUE))%>%
  addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = watersheds, group = "Hypsome-SWE", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, 
              label = ~NAME, labelOptions = labelOptions(textsize = '14px'), popup = paste0("<img src='https://data.climate.umt.edu/drought-indicators/plots/hypsome-swe-",
                                                                                            watersheds$HUC8,".png' height='500' width='500' loading='lazy'/>"),
              popupOptions = popupOptions(maxWidth ="auto", closeOnClick = TRUE))%>%
  addRasterImage(snodas_standardized_swe, colors = pal_standard, opacity = 0.8, group = "Standardized SWE", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("Standardized SWE"),
                            overlayGroups = c("SNOTEL (SWE)", 'Hypsome-SWE',"States",  "Weather", "USDM","Counties", 'Tribal Lands'),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal_rev, values = 50:150,
            title = paste0("SNOTEL<br>% Average SWE<br>", snotel_time),
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend(pal = pal_standard_r, values = -2.5:2.5,
            title = paste0("Standardized SWE<br>", snodas_time$time), 
            position = "bottomleft",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  leaflet::hideGroup(c("SNOTEL (SWE)", "Watersheds", "Counties", 'Tribal Lands'))%>%
  setView(lng = -108, lat = 46.5, zoom = 6) %>%
  prependContent(tags$style(type = "text/css", css_fix))

saveWidget(standardized_swe_map, paste0(export.dir, "widgets/m_raster_standardized_swe.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))

print('check -- ')

#standardized swe
standardized_swe_map_merged = base_map() %>%
  leaflet::addMapPane("SNOTEL (SWE)", zIndex = 420) %>%
  leaflet::addMapPane("Counties", zIndex = 410) %>%
  leaflet::addMapPane("Tribal Lands", zIndex = 410) %>%
  leaflet::addMapPane("Hypsome-SWE", zIndex = 410) %>%
  leaflet::addMapPane("USDM", zIndex = 400) %>%
  addCircleMarkers(snotel$longitude, snotel$latitude, snotel$sit_id,
                   popup = paste0("<img src='https://data.climate.umt.edu/drought-indicators/plots/snotel_plot_",
                                  snotel$site_id,".png' height='400' width='512' loading='lazy'/>",
                                  '<a target="_blank" rel="noopener noreferrer" href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/daily/',
                                  snotel$site_id,':',snotel$state,':SNTL/-7,0/WTEQ::value,SNWD::value,PREC::value,TOBS::value,TMAX::value,TMIN::value,TAVG::value">Current Daily SNOTEL Table (last 7 Days)</a><br>',
                                  '<a target="_blank" rel="noopener noreferrer" href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/hourly/',
                                  snotel$site_id,':',snotel$state,':SNTL/',Sys.Date(),',',Sys.Date(),'/WTEQ::value,SNWD::value,PREC::value,TOBS::value?sortBy=0:1">Current Hourly SNOTEL Table (Today)</a>'),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(snotel$swe_anom), group = "SNOTEL (SWE)", popupOptions = popupOptions(maxWidth ="auto", closeOnClick = TRUE))%>%
  addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = watersheds, group = "Hypsome-SWE", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1,
              label = ~NAME, labelOptions = labelOptions(textsize = '14px'), popup = paste0("<img src='https://data.climate.umt.edu/drought-indicators/plots/hypsome-swe-",
                                                                                            watersheds$HUC8,".png' height='500' width='500' loading='lazy'/>"),
              popupOptions = popupOptions(maxWidth ="auto", closeOnClick = TRUE))%>%
  addRasterImage(snodas_standardized_swe, colors = pal_standard, opacity = 0.8, group = "Standardized SWE", project = TRUE)%>%
  addRasterImage(current_swe, colors = pal_swe_raw, opacity = 0.8, group = "Current SWE", project = TRUE)%>%
  addRasterImage(current_depth, colors = pal_depth_raw, opacity = 0.8, group = "Current Depth", project = TRUE)%>%
  addRasterImage(current_1, colors = pal_r, opacity = 0.8, group = "24hr Change", project = TRUE)%>%
  addRasterImage(current_3, colors = pal_r, opacity = 0.8, group = "72hr Change", project = TRUE)%>%
  addRasterImage(current_7, colors = pal_r, opacity = 0.8, group = "7-Day Change", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("Standardized SWE", "Current SWE", 'Current Depth', "24hr Change", "72hr Change", "7-Day Change"),
                            overlayGroups = c("SNOTEL (SWE)", 'Hypsome-SWE',"States",  "Weather", "USDM","Counties", 'Tribal Lands'),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal_rev, values = 50:150,
            title = paste0("SNOTEL<br>% Average SWE<br>", snotel_time),
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend(pal = pal_standard_r, values = -2.5:2.5,
            title = paste0("Standardized SWE<br>", snodas_time$time),
            position = "bottomleft",
            group = "StandardizedSWE",
            className = "info legend StandardizedSWE",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  addLegend(pal = pal_depth_raw_rev, values = (min(values(current_depth), na.rm = T)-1):(max(values(current_depth), na.rm = T)+1),
            title = paste0("Current SNODAS<br> Snow Depth (in)<br>", current_depth_time),
            position = "bottomleft",
            group = "CurrentDepth",
            className = "info legend CurrentDepth",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  addLegend(pal = pal_swe_raw_rev, values = (min(values(current_swe), na.rm = T)-1):(max(values(current_swe), na.rm = T)+1),
            title = paste0("Current SNODAS<br> SWE Depth (in)<br>", current_swe_time),
            position = "bottomleft",
            group = "CurrentSWE",
            className = "info legend CurrentSWE",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  addLegend("bottomleft",
            pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",snodas_time$time),
            opacity = 1,
            group = "72hrChange",
            className = "info legend 72hrChange",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  addLegend("bottomleft", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",snodas_time$time),
            opacity = 1,
            group = "24hrChange",
            className = "info legend 24hrChange",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  addLegend("bottomleft", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",snodas_time$time),
            opacity = 1,
            group = "7-DayChange",
            className = "info legend 7-DayChange",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  leaflet::hideGroup(c("SNOTEL (SWE)", "Watersheds", "Counties", 'Tribal Lands'))%>%
  setView(lng = -108, lat = 46.5, zoom = 6) %>%
  prependContent(tags$style(type = "text/css", css_fix)) %>%
  #basemap event handeler to update legends
  htmlwidgets::onRender("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
            var selectedClass = selectedGroup.replace(' ', '');
            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedClass)) l.hidden=false;
            });
         };
         
         var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
         var selectedClass = selectedGroup.replace(' ', '');
         document.querySelectorAll('.legend').forEach(a => a.hidden=true);
         document.querySelectorAll('.legend').forEach(l => {
            if (l.classList.contains(selectedClass)) l.hidden=false;
         });
         
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }") #%>%
  
saveWidget(standardized_swe_map_merged, paste0(export.dir, "widgets/m_raster_standardized_swe_merged.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))
#saveWidget(standardized_swe_map_merged, '/home/zhoylman/Desktop/test.html', selfcontained = F, libdir = '/home/zhoylman/Desktop/libs')

# 
# #accumulated precipitaiton
# precip_map = base_map() %>%
#   addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
#   addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
#   addPolygons(data = watersheds, group = "Watersheds", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~NAME, labelOptions = labelOptions(textsize = '14px'))%>%
#   addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
#                    popup = paste0("<img src='https://data.climate.umt.edu/drought-indicators/plots/precip_snotel_plot_",
#                                   snotel$simple_id,".png' height='350' width='600' loading='lazy'/>"),
#                    radius = 10, stroke = TRUE, fillOpacity = 0.9,
#                    color = "black", fillColor = pal(daily_lookup$percent_precip)
#   )%>%
#   leaflet::addLayersControl(position = "topleft",
#                             overlayGroups = c("States",  "Weather", "USDM", "Counties", 'Watersheds', 'Tribal Lands'),
#                             options = leaflet::layersControlOptions(collapsed = FALSE)) %>%  
#   addLegend("bottomleft", pal = pal_rev, values = 50:150,
#             title = "% Average<br>Accumulated<br>Precipitation",
#             opacity = 1,
#             na.label = "NA",
#             labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
#   )%>%
#   setView(lng = -108, lat = 46.5, zoom = 6) %>%
#   leaflet::hideGroup(c("Watersheds", "Counties", 'Tribal Lands'))%>%
#   prependContent(tags$style(type = "text/css", css_fix))
# 
# saveWidget(precip_map, paste0(export.dir, "widgets/precip_snotel.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))
# 
# ##### Mobile SWE ######
library(htmltools)
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    position: bottomright;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Snow Depth Change & SNOTEL SWE")
)  

swe_map_mobile = base_map_mobile() %>%
  addControl(title, position = "bottomright", className="map-title")%>%
  addCircleMarkers(snotel$longitude, snotel$latitude, snotel$sit_id, 
                   popup = paste0("<img src='https://data.climate.umt.edu/drought-indicators/plots/snotel_plot_",
                                  snotel$site_id,".png' height='250' width='321' loading='lazy'/>", 
                                  '<a href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/daily/',
                                  snotel$site_id,':',snotel$state,':SNTL/-7,0/WTEQ::value,SNWD::value,PREC::value,TOBS::value,TMAX::value,TMIN::value,TAVG::value">Current Daily SNOTEL Table (last 7 Days)</a><br>',
                                  '<a href="https://wcc.sc.egov.usda.gov/reportGenerator/view/customSingleStationReport/hourly/',
                                  snotel$site_id,':',snotel$state,':SNTL/',Sys.Date()-1,',',Sys.Date(),'/WTEQ::value,SNWD::value,PREC::value,TOBS::value?sortBy=0:1">Current Hourly SNOTEL Table (Yesterday -> Today)</a>'),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(snotel$swe_anom), group = "SNOTEL (SWE)", options = popupOptions(maxWidth = 650))%>%
  addRasterImage(current_1, colors = pal_r, opacity = 0.8, group = "24hr Change", project = TRUE)%>%
  addRasterImage(current_3, colors = pal_r, opacity = 0.8, group = "72hr Change", project = TRUE)%>%
  addRasterImage(current_7, colors = pal_r, opacity = 0.8, group = "7 Day Change", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("24hr Change","72hr Change", "7 Day Change"),
                            overlayGroups = c("SNOTEL (SWE)",  "Weather", 'Legends'),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("topright", pal = pal_rev, values = 50:150,
            title = "SNOTEL<br>% Average<br>SWE (Daily)",
            opacity = 1,
            group = "Legends",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend("bottomleft", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",snodas_time$time),
            opacity = 1,
            group = "Legends",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  leaflet::hideGroup("Legends") %>%
  # addLegendNumeric(pal = pal_r_rev_numeric,
  #              values = -20:20,
  #              position = 'bottomright',
  #              title = paste0("SNODAS Snow <br>Depth Change (in)\n",snodas_time$time),
  #              orientation = 'horizontal',
  #              width = 100,
  #              height = 10,
  #              bins = 100,
  #              tickWidth = 20) %>%
  setView(lng = -113.990211, lat = 46.864089, zoom = 7)%>%
  onRender("function(el, x) {
    this.removeControl(this.zoomControl);
  }") %>%
  prependContent(tags$style(type = "text/css", css_fix)) 


saveWidget(swe_map_mobile, paste0(export.dir, "widgets/swe_snotel_mobile.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))

# #### standardized SWE


