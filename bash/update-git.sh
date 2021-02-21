#!/bin/bash

# Set some variables
DAY=$(date +%F);

# Make sure we run as root
if [ "$(whoami)" != "root" ]; then
    echo "Only root can do this.";
    exit 1;
else
    # Make sure we are in the right directory
    cd /home/zhoylman/mco-drought-indicators;
    # Now add any changes
    git add .;
    # Now commit
    git commit -m "$DAY Daily";
    git push origin master;
fi;
