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

build_html_raster = function(revalued_data, timescale_names, variable, title, color_palette, legend_values){
  m_raster = base_map()
  for(i in 1:length(timescale_names)){
    m_raster = m_raster %>%
      addRasterImage(revalued_data[[i]], colors = color_palette, opacity = 0.8, group = timescale_names[i], project = TRUE, layerId = timescale_names[i]) #%>%
      #leafem::addImageQuery(revalued_data[[i]],  group = timescale_names[i], layerId = timescale_names[i])
  } 
  
  m_raster = m_raster %>%
    addPolygons(data = counties, group = "Counties", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~COUNTY, labelOptions = labelOptions(textsize = '14px'))%>%
    addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~GNIS_Name1, labelOptions = labelOptions(textsize = '14px'))%>%
    addPolygons(data = watersheds, group = "Watersheds", fillColor = "transparent", weight = 0.5, color = "black", opacity = 1, label = ~NAME, labelOptions = labelOptions(textsize = '14px'))%>%
    addLayersControl(position = "topleft",
                     baseGroups = timescale_names,
                     overlayGroups = c("USDM", "States", "Weather", "Streets", "Counties", 'Watersheds', 'Tribal Lands'),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(c("Watersheds", "Counties", "Streets", 'Tribal Lands'))%>%
    addLegend(pal = color_palette, values = legend_values,
              title = title,
              position = "bottomleft")  %>%
    onRender(paste0("
      function(el, x) {
        this.removeControl(this.zoomControl);
        this.gestureHandling.enable();
        this.dragging.enable();
        
        var variable = '", variable, "'
        
        //fucntion to understand what the current layer selection is
        var baseLayerName = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
          var out = {name: selectedGroup};
          return out;
        };
       
        //compute today
        var today = new Date();
        var dd = String(today.getDate()).padStart(2, '0');
        var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
        var yyyy = today.getFullYear();
        today = mm + '/' + dd + '/' + yyyy;
        
        //set initial conditions
        var currentGroup = {name: '15 Day'}
        this.on('baselayerchange', el => currentGroup = baseLayerName());
        
        //define initail button for screen shot
        var button", variable, " = L.simpleMapScreenshoter({
            position: 'topleft', 
            hideElementsWithSelectors: ['.leaflet-top.leaflet-left'],
            caption:  '", variable, " [' + currentGroup.name + '] data from https://drought.climate.umt.edu/ - Contact: Montana Climate Office. Accessed on ' + today,
            captionFontSize: 18, 
            screenName: 'UMRB_drought_capture', 
            captionBgColor: 'black',
            captionColor: 'white'});
            
        //add to map
        button", variable, ".addTo(this)
        
        //upon a change of base map remove previous button
        this.on('baselayerchange', el => button", variable, ".remove())
        // and add a new button for the new basemap meta
        this.on('baselayerchange', el => button", variable, " = L.simpleMapScreenshoter({
                                          position: 'topleft', 
                                          hideElementsWithSelectors: ['.leaflet-top.leaflet-left'],
                                          caption:  '", variable, " [' + currentGroup.name + '] data from https://drought.climate.umt.edu/ - Contact: Montana Climate Office. Accessed on ' + today,
                                          captionFontSize: 18, 
                                          screenName: 'UMRB_drought_capture', 
                                          captionBgColor: 'black',
                                          captionColor: 'white'}).addTo(this))
        
      }"))
  
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