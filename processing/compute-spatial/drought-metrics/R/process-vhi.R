#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

vhi_files = read_table('https://satepsanone.nesdis.noaa.gov/pub/VHP/VHP1km/Weekly/j01/') %>%
  as_tibble()%>%
  filter(across(1, ~ grepl("VHI_geotiff", .))) %>%
  str_match(., "href=\"\\s*(.*?)\\s*.tif\"")

current_vhi = raster(paste0('/vsicurl/https://satepsanone.nesdis.noaa.gov/pub/VHP/VHP1km/Weekly/j01/', vhi_files[,2], '.tif')) %>%
  crop(., UMRB) %>%
  mask(., UMRB)
  
current_time = substr(vhi_files[,2], 57, 64) %>%
  as.Date(., format = '%Y%m%d')

writeRaster(current_vhi, '/home/zhoylman/mco-drought-indicators-data/vhi/current_vhi.tif', datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
writeRaster(current_vhi, '/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/current_vhi.tif', datatype='INT2S',  overwrite=TRUE, NAflag = -32767)

#write simple time txt
fileConn=file("/home/zhoylman/mco-drought-indicators-data/vhi/time.txt")
writeLines(current_time %>% as.character(), fileConn)
close(fileConn)
