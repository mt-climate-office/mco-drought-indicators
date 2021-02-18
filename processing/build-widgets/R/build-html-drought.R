#define git dir
git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#load all libraries for all apps
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

#load custom functions
source(paste0(git.dir, '/processing/ancillary-functions/R/widget-functions.R'))
source(paste0(git.dir, '/processing/ancillary-functions/R/base-map.R'))

#define variable names to process
variable = 'SPI'
lower_variable = 'spi'

#import counties
counties = st_read(paste0(git.dir, 'processing/base-data/processed/county_umrb.shp'))
watersheds = st_read(paste0(git.dir, 'processing/base-data/processed/watersheds_umrb.shp'))
tribal = st_read(paste0(git.dir, 'processing/base-data/processed/UMRB_tribal_lands_simple.geojson'))

#timescales 
timescales = c(30,60,90,180,365,'water_year', 'year_to_date')

#import data
files = list.files(paste0(export.dir,lower_variable), full.names = T) %>%
  as_tibble() %>%
  filter(stringr::str_detect(value, paste(timescales, sep = '', collapse = '|'))) 

#reorder filtered vector
data = files$value[sapply(timescales, function(x) { grep(x, files$value)})] %>%
  as_tibble() %>%
  as.list() %$%
  value %>%
  lapply(., raster) 

# get current time 
time = read_csv(paste0(export.dir,lower_variable, '/time.csv')) %$%
  time[1]

#compute median values for each vector geometry
watershed_vals = data %>%
  lapply(., aggregate_by_vector, vector = watersheds)

county_vals = data %>%
  lapply(., aggregate_by_vector, vector = counties)

tribal_vals = data %>%
  lapply(., aggregate_by_vector, vector = tribal)

#compute labels 
labels_tribal = list()
for(i in 1:length(tribal_vals)){
  labels_tribal[[i]] <- sprintf(
    "<strong>%s</strong><br/>%s = %g<sup></sup>",
    tribal$GNIS_Name1, rep(variable,length(tribal$GNIS_Name1)), tribal_vals[[i]]
  ) %>% lapply(htmltools::HTML)
}

labels_watershed = list()
for(i in 1:length(watershed_vals)){
  labels_watershed[[i]] <- sprintf(
    "<strong>%s</strong><br/>%s = %g<sup></sup>",
    watersheds$NAME, rep(variable,length(watersheds$NAME)),  watershed_vals[[i]]
  ) %>% lapply(htmltools::HTML)
}

labels_county = list()
for(i in 1:length(county_vals)){
  labels_county[[i]] <- sprintf(
    "<strong>%s</strong><br/>%s = %g<sup></sup>",
    counties$NAME, rep(variable,length(counties$NAME)), county_vals[[i]]
  ) %>% lapply(htmltools::HTML)
}

# revalue data
revalued_data = data %>%
  lapply(., revalue_raster_data, min = -2.5, max = 2.5)

revalued_watershed_vals = watershed_vals %>%
  lapply(., revalue_vector_data, min = -2.5, max = 2.5)

revalued_county_vals = county_vals %>%
  lapply(., revalue_vector_data, min = -2.5, max = 2.5)

revalued_tribal_vals = tribal_vals %>%
  lapply(., revalue_vector_data, min = -2.5, max = 2.5)

# make leaflet widgets
# define leaflet inputs
pal = colorNumeric(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), -2.5:2.5, na.color = "transparent")
timescale_names = c("30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")

# starting with raster data
m_raster = build_html_raster(revalued_data, timescale_names, variable, time)
saveWidget(m_raster, paste0("../widgets/m_raster_", lower_variable, ".html"), selfcontained = F, libdir = "../widgets/libs/")