#!/bin/bash

# Update Snotel Data
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/snotel/R/download-current-conditions.R >/home/zhoylman/mco-drought-indicators-data/logs/download-snotel 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/snotel/R/plot-current-conditions.R >/home/zhoylman/mco-drought-indicators-data/logs/plot-snotel 2>&1

# Update SNODAS data
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/snodas-maps.R >/home/zhoylman/mco-drought-indicators-data/logs/snodas 2>&1

# Update Map widgets
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-snotel-snodas.R >/home/zhoylman/mco-drought-indicators-data/logs/build-snodas-html 2>&1

#add transfer sh and git update sh here (after troubleshooting)
