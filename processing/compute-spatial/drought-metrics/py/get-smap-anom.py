try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO
import ee
import datetime as dt
from datetime import timedelta
import numpy
import csv

# initialize connection with earth engine
ee.Initialize()

#import states shape file
states = (ee.Collection.loadTable('users/zhoylman/states')
            .filter(ee.Filter.Or(ee.Filter.eq("STATE_ABBR",   'MT'),
                                  ee.Filter.eq("STATE_ABBR",   'ID'),
                                  ee.Filter.eq("STATE_ABBR",   'WY'),
                                  ee.Filter.eq("STATE_ABBR",   'SD'),
                                  ee.Filter.eq("STATE_ABBR",   'ND'),
                                  ee.Filter.eq("STATE_ABBR",   'OR'),
                                  ee.Filter.eq("STATE_ABBR",   'WA')))
          .union())

# import the dataset, select NDVI and clip to region
dataset = (ee.ImageCollection('NASA_USDA/HSL/SMAP10KM_soil_moisture')
           .select('ssma')
           .limit(1, 'system:time_start', False).first()
           .clip(states))

url_list = []

# Get a download URL for an image.
path = dataset.getDownloadUrl({
    'scale': 10000,
    'crs': 'EPSG:4326',
    'region': states.geometry()
})
url_list.append(path)

with open('/home/zhoylman/mco-drought-indicators-data/smap/urls/smap_url_list.csv', 'w') as myfile:
    wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    wr.writerow(url_list)
