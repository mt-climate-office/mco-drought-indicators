#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#umrb outline
outline = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#define timescales
timescales = c(7,15,30,60,90)

#pull in the urls to download greeness grids
urls = read_csv(paste0(export.dir, '/ndvi/urls/url_list_greeness_trend.csv'), col_names = F) %>%
  t()

for(i in 1:length(timescales)){
  GET(urls[i],
      write_disk(paste0(export.dir, 'ndvi/temp/temp_zip.zip'), overwrite = TRUE))
  
  data = unzip(paste0(export.dir, 'ndvi/temp/temp_zip.zip'), exdir = paste0(export.dir,'ndvi/temp/')) %>%
    raster::raster() %>%
    raster::mask(., outline)
  
  time = names(data) %>%
    substr(.,10,19) %>%
    as.Date(., format = "%Y.%m.%d")
  
  writeRaster(data, paste0(export.dir, 'data/ndvi_trend_', timescales[i], '.tif'), overwrite = T)
  
  #remove old zip and raster
  system(paste0('rm ', export.dir,'ndvi/temp/*'))
}