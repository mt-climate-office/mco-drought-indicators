#!/bin/bash

Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-drought.R 
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-precip-temp.R
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-cpc-and-grace.R
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-snotel-snodas.R 
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-ndvi.R   

cp -r /home/zhoylman/mco-drought-indicators-data/widgets/ /var/data/drought-indicators/
