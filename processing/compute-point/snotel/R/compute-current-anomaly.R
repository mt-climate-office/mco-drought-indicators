library(dplyr)
library(RCurl)
library(readr)
library(snotelr)
library(doParallel)
library(lubridate)

sites_with_climatology = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_climatology.csv')

sites = snotel_info() %>%
  filter(site_id %in% unique(sites_with_climatology$site_id))

get_snotel_most_recent = function(site_id, state, network){
  yday.waterYear = function(x, start.month = 10L){
    day = day(x)
    month = month(x)
    #dont want yday to go from 1 - 366, rather to 365
    new_date = make_date(2022, month, day)
    start.yr = year(new_date) - (month(new_date) < start.month)
    start.date = make_date(start.yr, start.month, 1L)
    as.integer(new_date - start.date + 1L)
  }
  
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport,metric/daily/",
    site_id[1], ":",
    state[1], ":",
    network[1],
    "%7Cid=%22%22%7Cname/",Sys.Date()-14,",",Sys.Date(),"/WTEQ::value,PREC::value"
  )
  
  export = getURL(base_url) %>%
    read_csv(skip = 59) %>%
    dplyr::filter(Date == max(Date)) %>%
    mutate(water_year_yday = yday.waterYear(Date),
           site_id = site_id) %>%
    dplyr::select(site_id, water_year_yday, Date, `Snow Water Equivalent (mm) Start of Day Values`, `Precipitation Accumulation (mm) Start of Day Values`)
  
  return(export)
}

print('starting snotel data dowload')

cl= makeCluster(4)
registerDoParallel(cl)

current = foreach(i = 1:length(sites$site_id), .packages = c('lubridate', 'dplyr', 'RCurl', 'readr')) %dopar% {
  tryCatch({
    get_snotel_most_recent(sites$site_id[i], sites$state[i], sites$network[i])
  }, error = function(e){
    tibble(site_id = NA, water_year_yday = NA, Date = NA,
           `Snow Water Equivalent (mm) Start of Day Values` = NA,
           `Precipitation Accumulation (mm) Start of Day Values` = NA)
  })
} %>%
  bind_rows() %>%
  tidyr::drop_na(site_id)

print('finsihed snotel data dowload')


stopCluster(cl)

print('starting snotel anomaly calculation')


anom = current %>%
  left_join(., sites_with_climatology, by = c('site_id', 'water_year_yday')) %>%
  mutate(swe_anom = 100* (`Snow Water Equivalent (mm) Start of Day Values`/swe_q50),
         precip_anom = 100* (`Precipitation Accumulation (mm) Start of Day Values`/precip_q50)) %>%
  dplyr::select(site_id, water_year_yday, Date, swe_anom, precip_anom)

write_csv(anom, '/home/zhoylman/mco-drought-indicators-data/snotel/anomaly/current_anomaly.csv')

print('finished snotel anomaly calculation')
