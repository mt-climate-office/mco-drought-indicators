#!/bin/bash

# Compute Current Drought Conditions
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/usdm-current.R >/home/zhoylman/mco-drought-indicators-data/logs/usdm 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/spi-map.R >/home/zhoylman/mco-drought-indicators-data/logs/spi 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/spei-map.R >/home/zhoylman/mco-drought-indicators-data/logs/spei 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/eddi-map.R >/home/zhoylman/mco-drought-indicators-data/logs/eddi 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/precipitation-map.R >/home/zhoylman/mco-drought-indicators-data/logs/precip 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/temperature-map.R >/home/zhoylman/mco-drought-indicators-data/logs/temp 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/get-cpc-and-grace.R >/home/zhoylman/mco-drought-indicators-data/logs/cpc-grace 2>&1

# Update Snotel Data
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/snotel/R/download-current-conditions.R >/home/zhoylman/mco-drought-indicators-data/logs/download-snotel 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/snotel/R/plot-current-conditions.R >/home/zhoylman/mco-drought-indicators-data/logs/plot-snotel 2>&1

# Update SNODAS data
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/snodas-maps.R >/home/zhoylman/mco-drought-indicators-data/logs/snodas 2>&1

# Build HTML widget files
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-drought.R >/home/zhoylman/mco-drought-indicators-data/logs/build-drought-html 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-precip-temp.R >/home/zhoylman/mco-drought-indicators-data/logs/build-precip-temp-html 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-cpc-and-grace.R >/home/zhoylman/mco-drought-indicators-data/logs/build-cpc-grace-html 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-snotel-snodas.R >/home/zhoylman/mco-drought-indicators-data/logs/build-snodas-html 2>&1

# Update Earth Engine data
python3 /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/py/ndvi-anom-modis.py
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/ndvi-anom-map.R
python3 /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/py/ndvi-trend-modis.py
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/ndvi-trend-map.R

# Build HTML widget files for Earth Engine datasets 
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-ndvi.R

#add transfer sh and git update sh here (after troubleshooting)
