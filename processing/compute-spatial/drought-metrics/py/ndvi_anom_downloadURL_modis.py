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

# define clipping fucntion to map over IC
def clipped(img):
    return img.clip(states)

# import the dataset, select NDVI and clip to region
dataset = (ee.ImageCollection('MODIS/MOD09GA_006_NDVI')
           .select('NDVI')
           .map(clipped))

# compute list of valid dates
dates = ee.List(dataset \
    .aggregate_array('system:time_start')) \
    .map(lambda time_start:
         ee.Date(time_start).format('Y-MM-dd')
    ).reverse() \
    .getInfo()

# compute tomorrow date for clipping
tomorrow_date = dt.datetime.strptime(dates[0], '%Y-%m-%d').date() + timedelta(days=1)

# define timescales of interest
timescale = [7,15,30,60,90]

# create list for storing download urls
url_list = []

for i in numpy.arange(0, len(timescale)):
    # compute current NDVI
    current = (dataset.filter(ee.Filter.date(dates[timescale[i] - 1], ee.Date(str(tomorrow_date))))
               .mean())

    # define julian day start and end for
    julian_end = (ee.Date(dates[0]).getRelative('day', 'year').getInfo())
    julian_start = (ee.Date(dates[timescale[i] - 1]).getRelative('day', 'year').getInfo())

    # clip dataset for julian days to compute zscore parameters
    julian_clip = (dataset.filter(ee.Filter.calendarRange(julian_start, julian_end)))

    # calcualte NDVI zscore and set metadata about dates considered
    z_score = ((current.subtract(julian_clip.mean())) \
               .divide(julian_clip.reduce(ee.Reducer.stdDev()))
               # .resample('bilinear')
               .set('time_end', str(dates[0]))
               .set('time_start', str(dates[timescale[i] - 1]))
               .rename(str(dates[0])+'_'+ str(timescale[i]) + 'day'))

    # Get a download URL for an image.
    path = z_score.getDownloadUrl({
        'scale': 4000,
        'crs': 'EPSG:4326',
        'region': states.geometry()
    })
    url_list.append(path)

with open('/home/zhoylman/drought_indicators/ndvi/data/urls/url_list.csv', 'w') as myfile:
    wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    wr.writerow(url_list)