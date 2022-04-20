#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#load all libraries for all apps
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#load custom functions
source(paste0(git.dir, '/processing/ancillary-functions/R/widget-functions.R'))
source(paste0(git.dir, '/processing/ancillary-functions/R/base-map.R'))

#define variable names to process
variable = c('Precipitation')
lower_variable = c('precipitation')

#import counties
counties = st_read(paste0(git.dir, 'processing/base-data/processed/county_umrb.shp'))
watersheds = st_read(paste0(git.dir, 'processing/base-data/processed/watersheds_umrb.shp'))
tribal = st_read(paste0(git.dir, 'processing/base-data/processed/UMRB_tribal_lands_simple.geojson'))
UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#timescales 
timescales = c(0.1,0.25,0.5)

# define leaflet inputs
pal_bins = colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                    domain = 0:100, bins = seq(0,100,10), na.color = "transparent")

pal_bins_reverse = colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                            domain = 0:364, bins = c(0,15,30,60,90,180,365,Inf), na.color = "transparent")

pal_bins_reverse_percentile = colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                            domain = 0:100, bins = seq(0,100,10), na.color = "transparent")


pal_reverse = colorNumeric(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), 0:365, na.color = "transparent")



timescale_names = c("Days Since 0.1in Event", "Days Since 0.25in Event", "Days Since 0.5in Event")

v = 1

#import data
days_since_files = list.files(paste0(export.dir,lower_variable[v]), full.names = T) %>%
  as_tibble() %>%
  filter(stringr::str_detect(value, 'days_since'))

percentile_since_files = list.files(paste0(export.dir,lower_variable[v]), full.names = T) %>%
  as_tibble() %>%
  filter(stringr::str_detect(value, 'percentile_since')) 

#reorder filtered vector
days_since_data = days_since_files$value[sapply(timescales, function(x) { grep(x, days_since_files$value)})] %>%
  as_tibble() %>%
  as.list() %$%
  value %>%
  lapply(., raster) %>%
  lapply(., mask, UMRB)

percentile_since_data = percentile_since_files$value[sapply(timescales, function(x) { grep(x, percentile_since_files$value)})] %>%
  as_tibble() %>%
  as.list() %$%
  value %>%
  lapply(., raster) %>%
  lapply(., mask, UMRB)

# get current time 
time = read_csv(paste0(export.dir,lower_variable[v], '/time.csv')) %$%
  time[1]

revalued_data_percentile = percentile_since_data %>%
  lapply(., revalue_raster_data, min = 0.1, max = 99.9)

revalued_data_days = days_since_data %>%
  lapply(., revalue_raster_data, min = 0.1, max = 364) %>%
  lapply(., function(x){export = x; values(export)[is.na(values(export))] = 730; return(export)})%>%
  lapply(., mask, UMRB)

#define legend title name (HTML)
title = paste0(variable[v], "<br>Percentile<br>", as.character(time))

library(leafem)

base_map() %>%
  addRasterImage(revalued_data_percentile[[1]], colors = pal_bins_reverse_percentile, opacity = 0.8, group = timescale_names[1], project = TRUE, layerId = timescale_names[1])%>%
  #addRasterImage(days_since_data[[1]], colors = pal_bins_reverse, opacity = 0.8, group = 'test', project = TRUE, layerId = 'values')%>%
  addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
  addPolygons(data = watersheds, group = "Watersheds", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~NAME, labelOptions = labelOptions(textsize = '14px'))%>%
  addLayersControl(position = "topleft",
                   baseGroups = timescale_names[1],
                   overlayGroups = c("USDM", "States", "Weather", "Streets", "Counties", 'Watersheds', 'Tribal Lands'),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  leaflet::hideGroup(c("Watersheds", "Counties", "Streets", 'Tribal Lands'))%>%
  addLegend(pal = pal_bins_reverse_percentile, values = seq(0,100,10),
            title = title,
            position = "bottomleft") %>%
  addMouseCoordinates()%>%
  addImageQuery(map = ., days_since_data[[1]], type="mousemove")


# make leaflet widgets
m_raster = build_html_raster(revalued_data_days, timescale_names, variable[v], title, pal_bins_reverse, legend_values = c(0,15,30,60,90,180,365,Inf))



saveWidget(m_raster, paste0(export.dir, "widgets/m_raster_", lower_variable[v], "_days_since.html"), selfcontained = F, libdir = paste0(export.dir, "widgets/libs/"))

