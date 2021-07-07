#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#import libs and functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#define base map information as a function used for all leaflet maps
#load base map dependent data
states = st_read(paste0(git.dir, 'processing/base-data/raw/states.shp'))
current_usdm = st_read(paste0(export.dir, "usdm/current_usdm.shp"))
current_usdm_date = read.csv(paste0(export.dir, "usdm/usdm_time.csv"))
current_usdm_date = as.Date(as.character(current_usdm_date$x), format = "%Y%m%d")
usdm_description = c("(Abnormally Dry)", "(Moderate Drought)",
                     "(Severe Drought)", "(Extreme Drought)",
                     "(Exceptional Drought)")

#google esk gesture control plugin
gesturePlugin = htmlDependency("Leaflet.GestureHandling", "1.1.8",
                               src = c(file = paste0(git.dir, "processing/manual-dependencies/Leaflet.GestureHandling-master/dist/")),
                               stylesheet = "leaflet-gesture-handling.min.css",
                               script = "leaflet-gesture-handling.min.js"
)

for(i in 1:length(current_usdm$DM)){
  current_usdm$DM1[i] = paste(current_usdm$DM[i], 
                              usdm_description[i], sep = " ")}

labels_usdm = list()
labels_usdm[[1]] <- sprintf(
  "<strong>%s</strong><br/>USDM = D%g<sup></sup>",
  current_usdm_date, current_usdm$DM
) %>% lapply(htmltools::HTML)

pal_usdm <- colorBin(colorRamp(c("#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616"), interpolate = "spline"), 
                     domain = 0:5, bins = seq(0,5,1))

pal_usdm_legend <- colorFactor(c("#ffff00", "#D2B48C", "#ffa500", "#ff0000", "#811616"), domain = c("D0 (Abnormally Dry)", "D1 (Moderate Drought)",
                                                                                                    "D2 (Severe Drought)", "D3 (Extreme Drought)",
                                                                                                    "D4 (Exceptional Drought)"))
# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

#define basemap function
base_map = function(x){
  leaflet::leaflet(options = leaflet::tileOptions(minZoom = 4, preferCanvas = T)) %>%
    registerPlugin(gesturePlugin) %>%
    leaflet::addMapPane("USDM", zIndex = 410) %>%
    leaflet::addProviderTiles("Stamen.Toner") %>%
    leaflet::addTiles("https://api.maptiler.com/tiles/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    leaflet::addProviderTiles("Stamen.TonerLines") %>%
    leaflet::addProviderTiles("Stamen.TonerLabels") %>%
    addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Streets") %>%
    leaflet::setMaxBounds( lng1 = -140
                           , lat1 = 60
                           , lng2 = -80
                           , lat2 = 30)%>%
    leaflet::setView(lng = -110, lat = 45.5, zoom = 6) %>%
    leaflet::addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
    leaflet::addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1, color = "black",
                         fillOpacity = 0.6, options = leaflet::pathOptions(pane = "USDM"), highlight = 
                           leaflet::highlightOptions(weight = 5,color = "#666",fillOpacity = 0.7),label = labels_usdm[[1]], 
                         labelOptions = leaflet::labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))%>%
    leaflet::addLegend("bottomright", group = "USDM", pal = pal_usdm_legend, values = c("D0 (Abnormally Dry)", "D1 (Moderate Drought)",
                                                                        "D2 (Severe Drought)", "D3 (Extreme Drought)",
                                                                        "D4 (Exceptional Drought)"),title = "USDM") %>%
    leaflet::addWMSTiles(
      "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", group = "Weather",
      layers = "nexrad-n0r-900913",
      options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE))%>%
    leaflet::addLayersControl(position = "topleft",
                              overlayGroups = c("USDM", "States", "Weather", "Streets"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(c("Weather", "Streets", 'USDM'))%>%
    leaflet.extras::addDrawToolbar(markerOptions = FALSE,
                                   polylineOptions = FALSE,
                                   polygonOptions = FALSE,
                                   circleOptions = FALSE,
                                   rectangleOptions = FALSE,
                                   circleMarkerOptions = FALSE,
                                   editOptions = FALSE,
                                   singleFeature = FALSE
    )%>%
    onRender("function(el, x) {
      this.removeControl(this.zoomControl);
      this.gestureHandling.enable();
      this.dragging.enable();
    }") 
}

#define base map information as a function used for all mobile leaflet maps
base_map_mobile = function(x){
  #prefer canvas works best for lots of points on mobile
  leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    leaflet::addTiles("https://api.maptiler.com/tiles/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    leaflet::addProviderTiles("Stamen.TonerLines") %>%
    leaflet::addProviderTiles("Stamen.TonerLabels") %>%
    leaflet::setView(lng = -105, lat = 45.5, zoom = 6) %>%
    leaflet::addPolygons(data = states, group = "States", fillColor = "transparent", weight = 5, color = "black", opacity = 1)%>%
    leaflet::addWMSTiles(
      "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", group = "Weather",
      layers = "nexrad-n0r-900913",
      options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE))%>%
    leaflet::addLayersControl(position = "topleft",
                              overlayGroups = c("States", "Weather"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup("Weather") %>%
    prependContent(tags$head(tags$meta(HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'))))
}
