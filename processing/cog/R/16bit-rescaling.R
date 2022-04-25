library(raster)
library(tidyverse)

rescale = function(x){
  temp = raster(x)*10
  return(temp)
}

names = list.files('/home/zhoylman/mco-drought-indicators-data/spi', full.names = F, pattern = "*.tif")
data = list.files('/home/zhoylman/mco-drought-indicators-data/spi', full.names = T, pattern = "*.tif") %>%
  lapply(., rescale)
  
for(i in 1:length(names)){
  writeRaster(data[[i]], paste0('/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/', names[i]), datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
}
