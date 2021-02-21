# snodas ingestion script and conversion to GeoTiff
# Zach Hoylman 2-18-2021

# snodas help info
# help https://nsidc.org/support/how/how-do-i-convert-snodas-binary-files-geotiff-or-netcdf
# naming table 4 https://nsidc.org/data/g02158#untar_daily_nc
# data ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/

library(httr)
library(dplyr)
library(data.table)

export.dir = '/home/zhoylman/mco-drought-indicators-data/'

get_snodas = function(date){
  for(d in 1:length(date)){
    #build data url
    url = paste0("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/", 
                 date[d] %>% format(., "%Y"), "/", date[d] %>% format(., "%m_%b"), 
                 "/SNODAS_", date[d] %>% format(., "%Y%m%d"), ".tar")
    
    #define where raw tarball will be stored
    tar.dir = paste0(export.dir,"snodas/raw/SNODAS_", date[d] %>% format(., "%Y%m%d"), ".tar")
    unzip.dir = paste0(export.dir, "snodas/raw/SNODAS_", date[d] %>% format(., "%Y%m%d"))
    
    #downlaod zipped data
    httr::GET(url, write_disk(path = tar.dir, overwrite=TRUE))
    
    #create unzip location
    dir.create(unzip.dir)
    
    #unzip file
    untar(tarfile = tar.dir,  exdir = unzip.dir)
    
    #get files from unzipped dir
    files = list.files(unzip.dir, full.names = T) 
    
    #define NOAA IDs for variables of interest
    files_of_interest = c("1034", "1036")
    
    for(i in 1:length(files_of_interest)) {
      file_to_process = files[[which(files %like% files_of_interest[i] & files %like% ".dat.gz")]]
      
      writeLines(
        "ENVI
samples = 6935
lines   = 3351
bands   = 1
header offset = 0
file type = ENVI Standard
data type = 2
interleave = bsq
byte order = 1", con = file_to_process %>% gsub(".dat.gz", ".hdr", .))
      
      file_to_process %>%
        R.utils::gunzip(., destname = gsub(".gz", "", .)) 
      
      processed_name = paste0(export.dir, "snodas/processed/",
                              if(files_of_interest[i] == "1034") paste0("swe/snodas_swe_conus_", format(date[d],"%Y%m%d"), ".tif") 
                              else if (files_of_interest[i] == "1036") paste0("snow_depth/snodas_snow_depth_conus_", format(date[d],"%Y%m%d"), ".tif") else NA)
      
      #different file dimentions after Oct. 2013
      if(date[d] > as.Date('2013-10-01')){
        print('Data from after Oct. 2013')
        system(paste0("gdal_translate -of GTiff -a_srs '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' -a_nodata -9999 -a_ullr  -124.73333333 52.87500000 -66.94166667 24.95000000 ",
                      file_to_process %>% gsub(".gz", "", .), " ",
                      processed_name))
      }
      else{
        print('Data from before Oct. 2013')
        system(paste0("gdal_translate -of GTiff -a_srs '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' -a_nodata -9999 -a_ullr  -124.73375000000000 52.87458333333333 -66.94208333333333 24.94958333333333 ",
                      file_to_process %>% gsub(".gz", "", .), " ",
                      processed_name))
      }
    }
    
    #erase raw data so we dont over store
    system(paste0("rm -r ", tar.dir))
    system(paste0("rm -r ", unzip.dir))
  }
}
