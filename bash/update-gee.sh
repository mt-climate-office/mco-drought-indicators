#!/bin/bash

#update GEE links
python3 /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/py/ndvi-anom-modis.py >/home/zhoylman/mco-drought-indicators-data/logs/ndvi-anom-py 2>&1
python3 /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/py/ndvi-trend-modis.py
python3 /home/zhoylman/mco-drought-indicators/processing/compute-spatial/drought-metrics/py/get-smap-anom.py >/home/zhoylman/mco-drought-indicators-data/logs/smap-anom-py 2>&1
