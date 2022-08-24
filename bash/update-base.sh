#!/bin/bash

# Compute Current Drought Conditions
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/usdm-current.R >/home/zhoylman/mco-drought-indicators-data/logs/usdm 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/spi-map.R >/home/zhoylman/mco-drought-indicators-data/logs/spi 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/spei-map.R >/home/zhoylman/mco-drought-indicators-data/logs/spei 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/eddi-map.R >/home/zhoylman/mco-drought-indicators-data/logs/eddi 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/precipitation-map.R >/home/zhoylman/mco-drought-indicators-data/logs/precip 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/temperature-map.R >/home/zhoylman/mco-drought-indicators-data/logs/temp 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/get-cpc-and-grace.R >/home/zhoylman/mco-drought-indicators-data/logs/cpc-grace 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/mesonet/soil-moisture-anomoly.R >/home/zhoylman/mco-drought-indicators-data/logs/mesonet-anom 2>&1

# Update Snotel Data
#Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/snotel/R/compute-current-anomaly.R >/home/zhoylman/mco-drought-indicators-data/logs/download-snotel 2>&1
#Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/snotel/R/plot-current-conditions.R >/home/zhoylman/mco-drought-indicators-data/logs/plot-snotel 2>&1

# Update SNODAS data
#Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/snodas-maps.R >/home/zhoylman/mco-drought-indicators-data/logs/snodas 2>&1

# Build HTML widget files
#Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-drought.R >/home/zhoylman/mco-drought-indicators-data/logs/build-drought-html 2>&1
#Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-precip-temp.R >/home/zhoylman/mco-drought-indicators-data/logs/build-precip-temp-html 2>&1
#Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-cpc-and-grace.R >/home/zhoylman/mco-drought-indicators-data/logs/build-cpc-grace-html 2>&1
#Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-snotel-snodas.R >/home/zhoylman/mco-drought-indicators-data/logs/build-snodas-html 2>&1

#process topofire data
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/process-topofire.R

#rescale data to 16 bit int.
Rscript /home/zhoylman/mco-drought-indicators/processing/cog/R/16bit-rescaling.R

#convert to cog
bash /home/zhoylman/mco-drought-indicators/bash/convert-to-cog.sh

# Transfer Files
bash /home/zhoylman/bash/transfer-drought-data.sh

# Update USDM Color Scheme Images
Rscript /home/zhoylman/mco-drought-indicators/processing/build-figures/R/usdm-color-scheme.R >/home/zhoylman/mco-drought-indicators-data/logs/build-usdm-color-scheme 2>&1
