library(raster)
library(dplyr)
library(magrittr)

rescale = function(x){
  temp = raster(x)*10
  return(temp)
}

#drougth indicies
names = list.files('/home/zhoylman/mco-drought-indicators-data/spi', full.names = F, pattern = "*.tif") %>%
  as_tibble() %>%
  bind_rows(as_tibble(list.files('/home/zhoylman/mco-drought-indicators-data/spei', full.names = F, pattern = "*.tif"))) %>%
  bind_rows(as_tibble(list.files('/home/zhoylman/mco-drought-indicators-data/eddi', full.names = F, pattern = "*.tif"))) %$%
  value

data = list.files('/home/zhoylman/mco-drought-indicators-data/spi', full.names = T, pattern = "*.tif") %>%
  as_tibble() %>%
  bind_rows(as_tibble(list.files('/home/zhoylman/mco-drought-indicators-data/spei', full.names = T, pattern = "*.tif"))) %>%
  bind_rows(as_tibble(list.files('/home/zhoylman/mco-drought-indicators-data/eddi', full.names = T, pattern = "*.tif"))) %$%
  value %>%
  lapply(., rescale)
  
for(i in 1:length(names)){
  writeRaster(data[[i]], paste0('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/', names[i]), datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
}


#precipititaion 

names = list.files('/home/zhoylman/mco-drought-indicators-data/precipitation', full.names = F, pattern = "*.tif") 

data = list.files('/home/zhoylman/mco-drought-indicators-data/precipitation', full.names = T, pattern = "*.tif") %>%
  lapply(., rescale)

for(i in 1:length(names)){
  writeRaster(data[[i]], paste0('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/precipitation_', names[i]), datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
}

#temperature 

names = list.files('/home/zhoylman/mco-drought-indicators-data/temperature', full.names = F, pattern = "*.tif") 

data = list.files('/home/zhoylman/mco-drought-indicators-data/temperature', full.names = T, pattern = "*.tif") %>%
  lapply(., rescale)

for(i in 1:length(names)){
  writeRaster(data[[i]], paste0('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/temperature_', names[i]), datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
}

#Soil Moisture 

names = list.files('/home/zhoylman/mco-drought-indicators-data/smap/data', full.names = F, pattern = "*.tif") 

data = list.files('/home/zhoylman/mco-drought-indicators-data/smap/data', full.names = T, pattern = "*.tif")  %>%
  lapply(., rescale)

for(i in 1:length(names)){
  writeRaster(data[[i]], paste0('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/', names[i]), datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
}


#GRACE groundwater

names = list.files('/home/zhoylman/mco-drought-indicators-data/grace', full.names = F, pattern = "*.tif") 

data = list.files('/home/zhoylman/mco-drought-indicators-data/grace', full.names = T, pattern = "*.tif")  %>%
  lapply(., rescale)

for(i in 1:length(names)){
  writeRaster(data[[i]], paste0('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/', names[i]), datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
}

#NDVI

names = list.files('/home/zhoylman/mco-drought-indicators-data/ndvi/data/anom', full.names = F, pattern = "*.tif") %>%
  as_tibble() %>%
  bind_rows(as_tibble(list.files('/home/zhoylman/mco-drought-indicators-data/ndvi/data/trend', full.names = F, pattern = "*.tif"))) %$%
  value

data = list.files('/home/zhoylman/mco-drought-indicators-data/ndvi/data/anom', full.names = T, pattern = "*.tif") %>%
  as_tibble() %>%
  bind_rows(as_tibble(list.files('/home/zhoylman/mco-drought-indicators-data/ndvi/data/trend', full.names = T, pattern = "*.tif"))) %$%
  value %>%
  lapply(., rescale)

for(i in 1:length(names)){
  writeRaster(data[[i]], paste0('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/', names[i]), datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
}

