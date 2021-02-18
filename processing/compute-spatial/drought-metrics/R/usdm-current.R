#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#download current USDM
#Function to extract date of USDM
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\d*")
}

UMRB = st_read(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp")) %>% st_set_precision(1e5) %>% st_make_valid()

#download USDM shp file
dlshape=function(shploc) {
  temp=tempfile()
  download.file(shploc, temp)
  files = unzip(temp)
  index = grep(".shp$", files)
  name = files[index]
  date_usdm = numextract(name)
  usdm = st_read(files[index]) %>% st_set_precision(1e5) %>% st_make_valid()
  unlink(temp)
  usdm_crop = st_intersection(usdm,UMRB)
  export = list()
  export[[1]] = usdm_crop
  export[[2]] = date_usdm
  return(export)
}

#Download
usdm = dlshape(shploc = "https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip")

#Export
path_file_usdm = paste0(export.dir, "USDM_current/current_usdm.shp")
#rgdal::writeOGR(obj=usdm[[1]], dsn=path_file_usdm, layer = "current_usdm", driver="ESRI Shapefile", overwrite_layer = T)
sf::st_write(usdm[[1]], dsn=path_file_usdm, layer = "current_usdm", delete_dsn = T)
#write time
write.csv(usdm[[2]], paste0(export.dir, "USDM_current/usdm_time.csv"))