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
path_file_usdm = paste0(export.dir, "usdm/current_usdm.shp")
path_file_usdm_geojson = paste0(export.dir, "usdm/current_usdm.geojson")

#rgdal::writeOGR(obj=usdm[[1]], dsn=path_file_usdm, layer = "current_usdm", driver="ESRI Shapefile", overwrite_layer = T)
sf::st_write(usdm[[1]], dsn=path_file_usdm, layer = "current_usdm", delete_dsn = T)

#add colors for flat geo buff
usdm_fgb = usdm[[1]] %>%
  mutate(fillColor = ifelse(DM == 0, '#FFFF00', 
                            ifelse(DM == 1, "#D2B48C", 
                                   ifelse(DM == 2, "#FFA500", 
                                          ifelse(DM == 3, "#FF0000", 
                                                 ifelse(DM == 4, "#811616", "#FFFFFF"))))))

sf::st_write(usdm_fgb, dsn=path_file_usdm_geojson, layer = "current_usdm", delete_dsn = T)

#write time
write.csv(usdm[[2]], paste0(export.dir, "usdm/usdm_time.csv"))

time = usdm[[2]] %>% as.Date(., format = "%Y%m%d") %>% as.character()

#write simple txt
fileConn<-file(paste0(export.dir, "usdm/time.txt"))
writeLines(time, fileConn)
close(fileConn)

# previous weeks USDM maps
process_historical_usdm = function(time, file_dsn){
  temp = st_read(paste0('https://droughtmonitor.unl.edu/data/json/usdm_',as.numeric(gsub("\\D", "", time)),'.json')) %>%
    st_make_valid() %>%
    st_intersection(., UMRB) %>%
    mutate(fillColor = ifelse(DM == 0, '#FFFF00', 
                              ifelse(DM == 1, "#D2B48C", 
                                     ifelse(DM == 2, "#FFA500", 
                                            ifelse(DM == 3, "#FF0000", 
                                                   ifelse(DM == 4, "#811616", "#FFFFFF"))))))
  
  sf::st_write(temp, dsn=file_dsn, layer = "historical_usdm", delete_dsn = T)
  
  return(temp)
}

#loop though the number of historical weeks of interest
for(i in 1:8){
  process_historical_usdm(time = as.Date(time) - 7 * i,
                          file_dsn = paste0("/home/zhoylman/mco-drought-indicators-data/usdm/historical_",i,"wk_usdm.geojson"))
}


