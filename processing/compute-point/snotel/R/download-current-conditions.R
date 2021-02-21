git.dir = '/home/zhoylman/mco-drought-indicators/'
export.dir = '/home/zhoylman/mco-drought-indicators-data/'

#load libs specific to snotel processing
source(paste0(git.dir, 'processing/compute-point/snotel/R/load-snotel-libs.R'))

#define input shp files
snotel = st_read(paste0(git.dir, "processing/base-data/snotel-data/Snotel_Sites.shp"))
states =  st_read(paste0(git.dir, "processing/base-data/raw/states.shp"))
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

#hit the NRCS server for current data
tic()
current = list()
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

current = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(RNRCS)
  tryCatch({
    grabNRCS.data("SNTL", as.numeric(snotel$site_num[i]), timescale = "daily", DayBgn = as.Date(Sys.Date()-3),
                                 DayEnd = as.Date(Sys.Date()))
  }, error = function(e){
    return(NA)
  }
  )
}

extract_columns <- function(data, collumn_name) {
  extracted_data <- data %>%
    dplyr::select_(.dots = collumn_name)
  return(extracted_data)
}

clusterExport(cl, "extract_columns")

current_select = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(dplyr)
  tryCatch({
    extract_columns(current[[i]], c("Date","Snow.Water.Equivalent..in..Start.of.Day.Values", "Precipitation.Accumulation..in..Start.of.Day.Values"))
  }, error = function(e){
    return(NA)
  }
  )
}
stopCluster(cl)

toc()

#clean up data and turn into data.frame
current_select_df = data.frame(matrix(nrow = length(current_select),
                                      ncol = 2))
for(i in 1:length(current_select)){
  tryCatch({
    current_select_df[i,1:2] = current_select[[i]] %>%
      tidyr::drop_na() %>%
      dplyr::filter(Date == max(Date))%>%
      dplyr::select(-Date)
    print(i)
  }, error = function(e){
    return(c(NA,NA))
  })
}

rownames(current_select_df) = snotel$site_name
colnames(current_select_df) = c("SWE", "Precip")

#load in climatology data
load(paste0(export.dir, "snotel/climatology/snotel_climatology.RData"))

#get current yday for lookup
current_yday = yday(as.Date(Sys.time()))

#compute percent of average
daily_lookup = data.frame(matrix(nrow=length(snotel$site_num), ncol = 2))
colnames(daily_lookup) = c("daily_mean_swe", "daily_mean_precip")

for(i in 1:length(snotel$site_num)){
  tryCatch({
    daily_lookup[i,1:2] = climatology[[i]] %>%
    dplyr::filter(yday == current_yday) %>%
    dplyr::select(median_swe, median_precip)
  }, error = function(e){
    return(c(NA,NA))
  }
  )
  
}

#merge
daily_lookup$current_swe = current_select_df$SWE
daily_lookup$current_precip = current_select_df$Precip

#make 0's 0.01
daily_lookup$daily_mean_swe[daily_lookup$daily_mean_swe == 0] = 0.01
daily_lookup$daily_mean_precip[daily_lookup$daily_mean_precip == 0] = 0.1

#compute pervent average
daily_lookup$percent_swe = (as.numeric(daily_lookup$current_swe)/as.numeric(daily_lookup$daily_mean_swe))*100
daily_lookup$percent_precip = (as.numeric(daily_lookup$current_precip)/as.numeric(daily_lookup$daily_mean_precip))*100

daily_lookup[daily_lookup == Inf] = NA
daily_lookup[daily_lookup == "NaN"] = NA

#add station id
daily_lookup$site_name = as.character(snotel$site_name)

#flatten list for export
daily_lookup = data.frame(daily_lookup)

#Write daily table
save(daily_lookup, file = paste0(export.dir, "snotel/climatology/current_precent_SWE.RData"))
