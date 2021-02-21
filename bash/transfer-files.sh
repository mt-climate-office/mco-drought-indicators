#!/bin/bash

# Copy widget and plot files to nginx folder
cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/widgets /srv/shiny-server/drought_indicators/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /srv/shiny-server/drought_indicators/snotel/ >/home/zhoylman/bash/log 2>&1

# Copy widgets and plots to git deployment folder
cp -r /home/zhoylman/drought_indicators/widgets /home/zhoylman/drought_indicators/docs/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /home/zhoylman/drought_indicators/docs/ 

#update git
/home/zhoylman/drought_indicators/bash/git_update.sh