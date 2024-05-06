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

#convert historical USDM
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_1wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_1wk_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_2wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_2wk_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_3wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_3wk_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_4wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_4wk_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_5wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_5wk_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_6wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_6wk_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_7wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_7wk_usdm.geojson
sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/historical_8wk_usdm.fgb /home/zhoylman/mco-drought-indicators-data/usdm/historical_8wk_usdm.geojson

#convert soil moisture geojsons to flat geo buffs

sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/current_soil_moisture_anom_shallow.fgb /home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_shallow.geojson

sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/current_soil_moisture_anom_middle.fgb /home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_middle.geojson

sudo ogr2ogr -f FlatGeobuf /var/data/drought-indicators/fgb/current_soil_moisture_anom_deep.fgb /home/zhoylman/mco-drought-indicators-data/mesonet/soil-moisture/geojson/current_soil_moisture_anom_deep.geojson

#copy data to server
sudo cp -r /home/zhoylman/mco-drought-indicators-data/cog/* /var/data/drought-indicators/cog/

#copy time files to fgb folder
sudo cp /home/zhoylman/mco-drought-indicators-data/usdm/time.txt /var/data/drought-indicators/fgb/usdm_time.txt
sudo cp /home/zhoylman/mco-drought-indicators-data/snotel/geojson/time.txt /var/data/drought-indicators/fgb/snotel_time.txt
