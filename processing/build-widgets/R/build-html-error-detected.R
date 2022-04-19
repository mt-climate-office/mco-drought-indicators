source('/home/zhoylman/mco-drought-indicators/processing/ancillary-functions/R/base-map.R')
  
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("An error was identified in the precipitation data used in this analysis. We are working to fix the problem as quickly as possible.")
)  

test = base_map() %>%
  addTiles() %>%
  addControl(title, position = "topleft", className="map-title")

saveWidget(test, "/home/zhoylman/mco-drought-indicators-data/widgets/m_raster_spi.html", selfcontained = F, libdir = paste0("/home/zhoylman/mco-drought-indicators-data/widgets/libs"))
saveWidget(test, "/home/zhoylman/mco-drought-indicators-data/widgets/m_raster_spei.html", selfcontained = F, libdir = paste0("/home/zhoylman/mco-drought-indicators-data/widgets/libs"))
saveWidget(test, "/home/zhoylman/mco-drought-indicators-data/widgets/m_raster_precipitation.html", selfcontained = F, libdir = paste0("/home/zhoylman/mco-drought-indicators-data/widgets/libs"))
