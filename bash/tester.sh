#!/bin/bash

Rscript /home/zhoylman/mco-drought-indicators/processing/compute-point/snotel/R/download-current-conditions.R >/home/zhoylman/mco-drought-indicators-data/logs/download-snotel 2>&1

Rscript /home/zhoylman/mco-drought-indicators/processing/build-widgets/R/build-html-snotel-snodas.R >/home/zhoylman/mco-drought-indicators-data/logs/build-snodas-html 2>&1
