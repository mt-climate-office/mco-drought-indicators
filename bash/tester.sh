#!/bin/bash

# Update SNODAS data
#Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/snodas-maps.R >/home/zhoylman/mco-drought-indicators-data/logs/snodas 2>&1

Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-snotel-snodas.R >/home/zhoylman/mco-drought-indicators-data/logs/build-snodas-html 2>&1

bash /home/zhoylman/bash/transfer-drought-data.sh
