nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

aggregate_by_vector = function(x, vector){
  # Extract raster values for each tribe 
  r.vals = exactextractr::exact_extract(x, vector %>% st_geometry(), fun = 'median', weights = NULL)
  # Use list apply to calculate median for each polygon
  r.median = lapply(r.vals, FUN=median,na.rm=TRUE) %>%
    lapply(., nullToNA) %>%
    unlist()
  return(r.median)
}

revalue_raster_data = function(x, min, max){
  values(x)[values(x) < min] = min+0.01
  values(x)[values(x) > max] = max-0.01
  return(x)
}

revalue_vector_data = function(x, min, max){
  x[x < min] = min+0.01
  x[x > max] = max-0.01
  return(x)
}

build_html_raster = function(revalued_data, timescale_names, variable, time){
  m_raster = base_map()
  for(i in 1:length(timescale_names)){
    m_raster = m_raster %>%
      addRasterImage(revalued_data[[i]], colors = pal, opacity = 0.8, group = timescale_names[i], project = TRUE, layerId = timescale_names[i]) 
  } 
  
  m_raster = m_raster%>%
    addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
    addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
    addPolygons(data = watersheds, group = "Watersheds", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~NAME, labelOptions = labelOptions(textsize = '14px'))%>%
    addLayersControl(position = "topleft",
                     baseGroups = timescale_names,
                     overlayGroups = c("USDM", "States", "Weather", "Streets", "Counties", 'Watersheds', 'Tribal Lands'),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(c("Watersheds", "Counties", "Streets", 'Tribal Lands'))%>%
    addLegend(pal = pal, values = -2.5:2.5,
              title = paste0("Current ", variable, "<br>", as.character(time)),
              position = "bottomleft") 
  return(m_raster)
}

build_html_vector = function(base_vector, revalued_data, labels, timescale_names, variable, time){
  m_vec = base_map()
  for(i in 1:length(timescale_names)){
    m_vec = m_vec %>% addPolygons(data = base_vector, group = timescale_names[i], fillColor = ~pal(revalued_data[[i]]), weight = 2, opacity = 1, color = "black", 
                                  dashArray = "3", fillOpacity = 0.7, highlight = 
                                    highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels[[i]], 
                                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
  }
  m_vec = m_vec %>%
    addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1)%>%
    addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
    addLayersControl(position = "topleft",
                     baseGroups = timescale_names,
                     overlayGroups = c("USDM", "States", "Weather", "Streets", "Counties", 'Tribal Lands'),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(c("Counties", "Streets", 'Tribal Lands')) %>%
    addLegend(pal = pal, values = -2.5:2.5,
              title = paste0("Current ", variable, "<br>", as.character(time)),
              position = "bottomleft")
  return(m_vec)
}