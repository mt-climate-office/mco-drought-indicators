#!/bin/bash

Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/ndvi-anom-map.R >/home/zhoylman/mco-drought-indicators-data/logs/ndvi-anom-map 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/ndvi-trend-map.R >/home/zhoylman/mco-drought-indicators-data/logs/ndvi-trend-map 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/smap-map.R >/home/zhoylman/mco-drought-indicators-data/logs/smap-map 2>&1

# Build HTML widget files for Earth Engine datasets 
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-ndvi.R >/home/zhoylman/mco-drought-indicators-data/logs/build-html-ndvi 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-smap.R >/home/zhoylman/mco-drought-indicators-data/logs/build-html-smap 2>&1


