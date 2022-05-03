#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))
source(paste0(git.dir,"/processing/ancillary-functions/R/drought-functions.R"))

#umrb outline
outline = readOGR(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp"))

#pull in the urls to download greeness grids
urls = read_csv(paste0(export.dir, '/smap/urls/smap_url_list.csv'), col_names = F) %>%
  t()

GET(urls[1],
    write_disk(paste0(export.dir, 'smap/temp/temp_zip.zip'), overwrite = TRUE))

data = unzip(paste0(export.dir, 'smap/temp/temp_zip.zip'), exdir = paste0(export.dir,'smap/temp/')) %>%
  raster::raster() %>%
  raster::mask(., outline)

time = names(data) %>%
  substr(.,27,34) %>%
  as.Date(., format = "%Y%m%d")

writeRaster(data, paste0(export.dir, 'smap/data/current_smap_anom.tif'), overwrite = T)

#remove old zip and raster
system(paste0('rm ', export.dir,'smap/temp/*'))

#write time out
out.time = data.frame(time = time)
write.csv(out.time, paste0(export.dir, "smap/data/time.csv"))

#write simple txt
fileConn<-file(paste0(export.dir, "smap/data/time.txt"))
writeLines(time %>% as.character(), fileConn)
close(fileConn)