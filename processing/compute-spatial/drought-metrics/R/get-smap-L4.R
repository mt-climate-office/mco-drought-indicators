#load required libraries
library(reticulate) # allows for python interfacing
library(rgee) # R wrapper for the python GEE library
library(sf) # simple feature library - used for vectors
library(dplyr) # package for tidy syntax etc
library(geojsonio) # package to send ROI SF objects to GEE

#set up the gee environment
use_condaenv("gee-base", conda = "auto",required = TRUE)
ee = import("ee")
ee_Initialize(drive = TRUE)

#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

ee_roi = st_read(paste0(git.dir, "/processing/base-data/processed/outline_umrb.shp")) %>%
  st_geometry() %>%
  sf_as_ee()

smap_rootzone = ee$ImageCollection("NASA/SMAP/SPL4SMGP/007")$
  select('sm_rootzone_pctl')$
  limit(1, 'system:time_start', F)$
  first()$
  clip(ee_roi)
  
  
date = ee_get_date_img(smap_rootzone, time_end = FALSE)$time_start %>%
  as.Date()

smap_data = ee_as_raster(
  smap_rootzone,
  region = ee_roi,
  scale = 11000
)
