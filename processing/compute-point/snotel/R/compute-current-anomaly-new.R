library(dplyr)
library(RCurl)
library(readr)
library(snotelr)
library(doParallel)
library(lubridate)
library(leaflet)
library(sf)
library(xml2)

library(httr2)

get_snotel_inventory <- 
  function(){
    httr2::request("https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/stations") |>
      httr2::req_url_query(stationTriplets='*:*:SNTL') |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      purrr::map_dfr(tibble::as_tibble) |>
      sf::st_as_sf(coords = c("longitude", "latitude"),
                   crs = 4326)
  }

get_snotel_inventory()


get_snotel_station <-
  function(stationTriplets = '916:MT:SNTL',
           beginDate = '1900-01-01',
           endDate = '2100-01-01',
           elements = c('WTEQ','PREC'),
           duration = 'DAILY'){
    
    dat <-
      httr2::request("https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/data") |>
      httr2::req_url_query(
        beginDate = beginDate,
        endDate = endDate,
        elements = elements,
        stationTriplets = stationTriplets,
        duration = duration,
        .multi = "comma"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
    units <-
      dat[[1]]$data |>
      purrr::map_dfr(
        ~(
          .x$stationElement |>
            tibble::as_tibble()
        )
      )
    
    units <-
      units$storedUnitCode |>
      magrittr::set_names(units$elementCode)
    
    dat[[1]]$data |>
      purrr::map_dfr(
        ~(
          .x$values |>
            purrr::map_dfr(tibble::as_tibble) |>
            dplyr::mutate(element = .x$stationElement$elementCode)
        )
      ) |>
      tidyr::pivot_wider(names_from = element,
                         values_from = value)
  }

get_snotel_station()



yday.waterYear = function(x, start.month = 10L){
  day = day(x)
  month = month(x)
  #dont want yday to go from 1 - 366, rather to 365
  new_date = make_date(2022, month, day)
  start.yr = year(new_date) - (month(new_date) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(new_date - start.date + 1L)
}

sites_with_climatology = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_climatology.csv')

sites = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_meta.csv') %>%
  filter(site_id %in% unique(sites_with_climatology$site_id))

#i = 1
#site_id = sites$site_id[i]; state = sites$state[i]; network = sites$network[i]

get_snotel_most_recent = function(site_id, state, network){
  yday.waterYear = function(x, start.month = 10L){
    day = day(x)
    month = month(x)
    #dont want yday to go from 1 - 366, rather to 365 =
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
    "/",Sys.Date()-14,",",Sys.Date(),"/WTEQ::value,PREC::value"
    #"%7Cid=%22%22%7Cname/",Sys.Date()-14,",",Sys.Date(),"/WTEQ::value,PREC::value"
  )
  
  export = getURL(base_url) %>%
    gsub(".*As of:","",.) %>%
    read_csv(., skip = 2) %>%
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
    temp = get_snotel_most_recent(sites$site_id[i], sites$state[i], sites$network[i])
    if(length(temp$site_id) == 0){
      temp = tibble(site_id = sites$site_id[i], water_year_yday = as.numeric(NA), Date = Sys.Date(),
                    `Snow Water Equivalent (mm) Start of Day Values` = as.numeric(NA),
                    `Precipitation Accumulation (mm) Start of Day Values` = as.numeric(NA))
    } 
    temp
  }, error = function(e){
    tibble(site_id = sites$site_id[i], water_year_yday = as.numeric(NA), Date = Sys.Date(),
           `Snow Water Equivalent (mm) Start of Day Values` = as.numeric(NA),
           `Precipitation Accumulation (mm) Start of Day Values` = as.numeric(NA))
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

pal = colorNumeric(c("red", "yellow", "white", "cyan", "blue"), domain = c(50, 150), na.color = "#808080")

snotel_geojson = sites %>%
  left_join(anom, by = c('site_id'))%>%
  mutate(swe_anom = ifelse(swe_anom > 150, 149, swe_anom),
         swe_anom = ifelse(swe_anom <= 50, 51, swe_anom),
         precip_anom = ifelse(precip_anom > 150, 149, precip_anom),
         fillColor = pal(swe_anom)) %>%
  st_as_sf(., coords = c('longitude', 'latitude')) %>%
  st_set_crs(st_crs('EPSG:4326'))

st_write(snotel_geojson, '/home/zhoylman/mco-drought-indicators-data/snotel/geojson/current_snotel.geojson', delete_dsn = T,append=FALSE)

#write simple txt
fileConn<-file("/home/zhoylman/mco-drought-indicators-data/snotel/geojson/time.txt")
writeLines(anom$Date[1] %>% as.character(), fileConn)
close(fileConn)

print('finished snotel anomaly calculation')

