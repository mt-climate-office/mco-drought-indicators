#!/bin/bash

# install required dependencies
# this was troubleshooted on Ubuntu 20.04.2 Standard Install
#netcdf dependendencies
sudo apt-get install libnetcdf-dev libnetcdff-dev
sudo apt install netcdf-bin
#gdal dependencies
sudo apt-get install gdal-bin libgdal-dev
#sf dependencies (specifically units)
sudo apt-get install libudunits2-dev

# install libraries to the root enviorment
sudo su - -c "R -q -e \"install.packages('R.utils', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('raster', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('ncdf4', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('lmomco', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('doParallel', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('foreach', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('rgdal', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('stringr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('sf', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('lubridate', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('magrittr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('httr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('readr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"devtools::install_version('leaflet', version = '2.0.2',  repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('leaflet.extras', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('htmltools', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('htmlwidgets', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('XML', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('RNRCS', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('tictoc', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('data.table', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -q -e \"install.packages('flexdashboard', repos='http://cran.rstudio.com/')\""
