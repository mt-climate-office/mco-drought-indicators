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

#define input shp files
snotel = st_read(paste0(git.dir, "processing/base-data/snotel-data/Snotel_Sites.shp"))
states =  st_read(paste0(git.dir, "processing/base-data/raw/states.shp"))
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

#Standardized swe import grid
snodas_standardized_swe = raster(paste0(export.dir, 'snodas/processed/standardized_swe/current_snodas_swe_standardized.tif'))

snodas_standardized_swe[snodas_standardized_swe >= 2.5] = 2.49
snodas_standardized_swe[snodas_standardized_swe <= -2.5] = -2.49

#load current conditions 
load(paste0(export.dir, "snotel/climatology/current_precent_SWE.RData"))

daily_lookup$percent_swe[daily_lookup$percent_swe > 150] = 150 
daily_lookup$percent_precip[daily_lookup$percent_precip > 150] = 150 

daily_lookup$percent_swe[daily_lookup$percent_swe < 50] = 50 
daily_lookup$percent_precip[daily_lookup$percent_precip < 50] = 50 

daily_lookup$percent_swe[daily_lookup$percent_swe == "NaN"] = NA
daily_lookup$percent_precip[daily_lookup$percent_precip == "NaN"] = NA

na.index = which(is.na(daily_lookup$daily_mean_swe), arr.ind=TRUE)

snotel$lat[na.index] = NA
snotel$lon[na.index] = NA

#color pallet
pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(daily_lookup$percent_swe, na.rm = T),max(daily_lookup$percent_swe, na.rm = T)), na.color = "grey")
pal_standard <- colorNumeric(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), -2.5:2.5, na.color = "transparent")
pal_standard_r <- colorNumeric(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), -2.5:2.5, na.color = "transparent")

#time id
time = read.csv(paste0(export.dir, 'snodas/processed/standardized_swe/time.csv')) 

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

#custom legend fix
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

swe_map = base_map() %>%
  leaflet::addMapPane("SNOTEL (SWE)", zIndex = 420) %>%
  leaflet::addMapPane("Counties", zIndex = 410) %>%
  leaflet::addMapPane("Tribal Lands", zIndex = 410) %>%
  leaflet::addMapPane("Watersheds", zIndex = 410) %>%
  leaflet::addMapPane("USDM", zIndex = 400) %>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                   popup = paste0("<img src='../plots/snotel_plot_",
                                  snotel$simple_id,".png' height='350' width='600' loading='lazy'/>"),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(daily_lookup$percent_swe), group = "SNOTEL (SWE)")%>%
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
            title = "SNOTEL<br>% Average<br>SWE (Daily)",
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend("bottomright", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",time$time),
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
  leaflet::addMapPane("Watersheds", zIndex = 410) %>%
  leaflet::addMapPane("USDM", zIndex = 400) %>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                   popup = paste0("<img src='../plots/snotel_plot_",
                                  snotel$simple_id,".png' height='350' width='600' loading='lazy'/>"),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(daily_lookup$percent_swe), group = "SNOTEL (SWE)")%>%
  addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = watersheds, group = "Watersheds", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~NAME, labelOptions = labelOptions(textsize = '14px'))%>%
  addRasterImage(snodas_standardized_swe, colors = pal_standard, opacity = 0.8, group = "Standardized SWE", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("Standardized SWE"),
                            overlayGroups = c("SNOTEL (SWE)", "States",  "Weather", "USDM","Counties", 'Watersheds', 'Tribal Lands'),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal_rev, values = 50:150,
            title = "SNOTEL<br>% Average<br>SWE (Daily)",
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend(pal = pal_standard_r, values = -2.5:2.5,
            title = paste0("Standardized SWE<br>", time$time), 
            position = "bottomleft",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
  leaflet::hideGroup(c("SNOTEL (SWE)", "Watersheds", "Counties", 'Tribal Lands'))%>%
  setView(lng = -108, lat = 46.5, zoom = 6) %>%
  prependContent(tags$style(type = "text/css", css_fix))

saveWidget(standardized_swe_map, paste0(export.dir, "widgets/m_raster_standardized_swe.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))

#accumulated precipitaiton
precip_map = base_map() %>%
  addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = watersheds, group = "Watersheds", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~NAME, labelOptions = labelOptions(textsize = '14px'))%>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                   popup = paste0("<img src='../plots/precip_snotel_plot_",
                                  snotel$simple_id,".png' height='350' width='600' loading='lazy'/>"),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(daily_lookup$percent_precip)
  )%>%
  leaflet::addLayersControl(position = "topleft",
                            overlayGroups = c("States",  "Weather", "USDM", "Counties", 'Watersheds', 'Tribal Lands'),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%  
  addLegend("bottomleft", pal = pal_rev, values = 50:150,
            title = "% Average<br>Accumulated<br>Precipitation",
            opacity = 1,
            na.label = "NA",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  setView(lng = -108, lat = 46.5, zoom = 6) %>%
  leaflet::hideGroup(c("Watersheds", "Counties", 'Tribal Lands'))%>%
  prependContent(tags$style(type = "text/css", css_fix))

saveWidget(precip_map, paste0(export.dir, "widgets/precip_snotel.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))

##### Mobile SWE ######
swe_map_mobile = base_map_mobile() %>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, group = "SNOTEL (SWE)",
                   popup = paste0("<img src='../snotel/plots/snotel_plot_mobile_",
                                  snotel$simple_id,".png' height='200' width='270' loading='lazy'/>"),
                   radius = 12, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(daily_lookup$percent_swe)
  )%>%
  addRasterImage(current_1, colors = pal_r, opacity = 0.8, group = "24hr Change", project = TRUE)%>%
  addRasterImage(current_3, colors = pal_r, opacity = 0.8, group = "72hr Change", project = TRUE)%>%
  addRasterImage(current_7, colors = pal_r, opacity = 0.8, group = "7 Day Change", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("24hr Change","72hr Change", "7 Day Change"),
                            overlayGroups = c("SNOTEL (SWE)", "States",  "Weather"),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal_rev, values = 50:150,
            title = "SNOTEL<br>% Average<br>SWE (Daily)",
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend("bottomright", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",time$time),
            opacity = 1,
            group = "24hr Snow Change",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  setView(lng = -113.990211, lat = 46.864089, zoom = 7)%>%
  onRender("function(el, x) {
    this.removeControl(this.zoomControl);
  }") %>%
  prependContent(tags$style(type = "text/css", css_fix))

saveWidget(swe_map_mobile, paste0(export.dir, "widgets/swe_snotel_mobile.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))

#### standardized SWE


