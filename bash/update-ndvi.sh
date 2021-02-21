#!/bin/bash

#update modis ndvi anom
python3 /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/py/ndvi_anom_modis.py >/home/zhoylman/mco-drought-indicators-data/logs/ndvi-anom-py 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/ndvi-anomoly-map.R >/home/zhoylman/mco-drought-indicators-data/logs/ndvi-anom-R 2>&1

#update modis ndvi trend
python3 /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/py/ndvi_trend_modis.py >/home/zhoylman/mco-drought-indicators-data/logs/ndvi-trend-py 2>&1
Rscript /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/R/ndvi-trend-map.R >/home/zhoylman/mco-drought-indicators-data/logs/ndvi-trend-R 2>&1

#build ndvi widgets
Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-ndvi.R >/home/zhoylman/mco-drought-indicators-data/logs/build-html-ndvi 2>&1

#fin
