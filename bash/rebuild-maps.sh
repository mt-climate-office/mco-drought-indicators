#!/bin/bash

Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-drought.R 
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-precip-temp.R
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-cpc-and-grace.R
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-snotel-snodas.R 
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-ndvi.R  
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-smap.R >/home/zhoylman/mco-drought-indicators-data/logs/build-html-smap 2>&1

cp -r /home/zhoylman/mco-drought-indicators-data/widgets/ /var/data/drought-indicators/
