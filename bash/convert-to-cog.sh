#!/bin/bash


search_dir=/home/zhoylman/mco-drought-indicators-data/16bit-rescaled
gdal_base_str="gdal_translate"
gdal_end="-of COG -co COMPRESS=DEFLATE"
out_dir="/home/zhoylman/mco-drought-indicators-data/cog/"

for entry in "$search_dir"/*
do
  ${gdal_base_str} ${entry} ${out_dir}$(basename ${entry}) ${gdal_end}
done

sudo cp -r /home/zhoylman/mco-drought-indicators-data/cog/* /var/data/drought-indicators/cog/
