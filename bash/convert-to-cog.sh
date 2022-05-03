#!/bin/bash

#convert tifs to COGs
search_dir=/home/zhoylman/mco-drought-indicators-data/16bit-rescaled
gdal_base_str="gdal_translate"
gdal_end="-of COG -co COMPRESS=DEFLATE"
out_dir="/home/zhoylman/mco-drought-indicators-data/cog/"

for entry in "$search_dir"/*
do
  ${gdal_base_str} ${entry} ${out_dir}$(basename ${entry}) ${gdal_end}
done

#convert USDM to fgb
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/current_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/current_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/current_snotel.fgb /home/zhoylman/mco-drought-indicators-data/snotel/geojson/current_snotel.geojson

#copy data to server
sudo cp -r /home/zhoylman/mco-drought-indicators-data/cog/* /var/data/drought-indicators/cog/

#copy time files to fgb folder
sudo cp /home/zhoylman/mco-drought-indicators-data/usdm/time.txt /var/data/drought-indicators/fgb/usdm_time.txt
sudo cp /home/zhoylman/mco-drought-indicators-data/snotel/geojson/time.txt /var/data/drought-indicators/fgb/snotel_time.txt
