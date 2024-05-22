#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

UMRB = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

vhi_files = read_table('https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/') %>%
  as_tibble(.)%>%
  filter(across(1, ~ grepl("VH.VHI.tif", .))) %>%
  tail(1) %>%
  mutate(name = str_match(., 'href="\\s*(.*?)\\s*.tif')[,2]) %$%
  name

current_vhi = raster(paste0('/vsicurl/https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/', vhi_files, '.tif')) %>%
  crop(., UMRB) %>%
  mask(., UMRB)

current_vhi[current_vhi<=0] = NA
  
current_time = substr(vhi_files, 18, 24) %>%
  as.Date(., format = '%Y0%W') - 3

writeRaster(current_vhi, '/home/zhoylman/mco-drought-indicators-data/vhi/current_vhi.tif', datatype='INT2S',  overwrite=TRUE, NAflag = -32767)
writeRaster(current_vhi, '/home/zhoylman/mco-drought-indicators-data/16bit-rescaled/current_vhi.tif', datatype='INT2S',  overwrite=TRUE, NAflag = -32767)

#write simple time txt
fileConn=file("/home/zhoylman/mco-drought-indicators-data/vhi/time.txt")
writeLines(current_time %>% as.character(), fileConn)
close(fileConn)
