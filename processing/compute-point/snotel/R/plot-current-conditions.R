library(dplyr)
library(RCurl)
library(readr)
library(snotelr)
library(doParallel)
library(lubridate)
library(stringr)
library(ggplot2)
library(magrittr)

sites_with_climatology = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_climatology.csv')
site_meta = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_meta.csv')

sites = snotel_info() %>%
  filter(site_id %in% unique(sites_with_climatology$site_id))

# get start year based on water year
base_month = Sys.Date() %>% lubridate::month()
if(base_month %in% c(1:9)){
  base_year = Sys.Date() %>% lubridate::year() - 1
} else {
  base_year = Sys.Date() %>% lubridate::year()
}

get_snotel_water_year = function(site_id, state, network){
  yday.waterYear = function(x, start.month = 10L){
    day = day(x)
    month = month(x)
    #dont want yday to go from 1 - 366, rather to 365
    new_date = make_date(2023, month, day)
    start.yr = year(new_date) - (month(new_date) < start.month)
    start.date = make_date(start.yr, start.month, 1L)
    as.integer(new_date - start.date + 1L)
  }
  
  # get start year based on water year
  base_month = Sys.Date() %>% lubridate::month()
  if(base_month %in% c(1:9)){
    base_year = Sys.Date() %>% lubridate::year() - 1
  } else {
    base_year = Sys.Date() %>% lubridate::year()
  }
  
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport,metric/daily/",
    site_id[1], ":",
    state[1], ":",
    network[1],
    "%7Cid=%22%22%7Cname/",paste0(base_year, '-10-01'),",",Sys.Date(),"/WTEQ::value,PREC::value"
  )
  
  export = getURL(base_url) %>%
    read_csv(skip = 59) %>%
    mutate(water_year_yday = yday.waterYear(Date),
           site_id = site_id) %>%
    dplyr::select(site_id, water_year_yday, Date, `Snow Water Equivalent (mm) Start of Day Values`, `Precipitation Accumulation (mm) Start of Day Values`)
  
  return(export)
}

print('starting snotel download')

cl= makeCluster(4)
registerDoParallel(cl)

current = foreach(i = 1:length(sites$site_id), .packages = c('lubridate', 'dplyr', 'RCurl', 'readr')) %dopar% {
  tryCatch({
    get_snotel_water_year(sites$site_id[i], sites$state[i], sites$network[i])
  }, error = function(e){
    tibble(site_id = NA, water_year_yday = NA, Date = NA,
           `Snow Water Equivalent (mm) Start of Day Values` = NA,
           `Precipitation Accumulation (mm) Start of Day Values` = NA)
  })
  
}%>%
  lapply(., function(x){x %>% mutate(Date = as.Date(Date),
                                     `Snow Water Equivalent (mm) Start of Day Values` = as.numeric(`Snow Water Equivalent (mm) Start of Day Values`),
                                     `Precipitation Accumulation (mm) Start of Day Values` = as.numeric(`Precipitation Accumulation (mm) Start of Day Values`))}) %>%
  bind_rows()%>%
  tidyr::drop_na(site_id)

stopCluster(cl)

print('finished snotel download')

plot_snotel = function(current_data, climatology_data, site_id_, base_year){
  #tryCatch({
  data_select = current_data %>%
    filter(site_id == site_id_)
  
  climatology_start_year = site_meta %>%
    filter(site_id == site_id_) %$%
    year
  
  climatology_data_select = climatology_data  %>%
    filter(site_id == site_id_) %>%
    mutate(WY_date = ISOdate(ifelse(month %in% c(10:12), base_year, base_year+1), month, day) %>%
             as.Date())
  
  site_meta = sites %>%
    filter(site_id == site_id_)
  
  plot = ggplot()+
    ggtitle(paste0(str_to_title(site_meta$site_name), " (", as.Date(max(data_select$Date)), ")"))+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q95, ymax = swe_max, fill = "95th - Max"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q75, ymax = swe_q95, fill = "75th - 95th"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q25, ymax = swe_q75, fill = "25th - 75th"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_q05, ymax = swe_q25, fill = "5th - 25th"), alpha = 0.25)+
    geom_ribbon(data = climatology_data_select, aes(x = WY_date, ymin = swe_min, ymax = swe_q05, fill = "Min - 5th"), alpha = 0.25)+
    geom_line(data = climatology_data_select, aes(x = WY_date, y = swe_min), color = 'darkred', size = 0.75)+
    geom_line(data = climatology_data_select, aes(x = WY_date, y = swe_max), color = 'darkblue', size = 0.75)+
    geom_line(data = climatology_data_select, aes(x = WY_date, y = swe_q50, color = "Median"), size = 0.75)+
    geom_line(data = data_select, aes(x = Date, y = `Snow Water Equivalent (mm) Start of Day Values`, color = "Current"), size = 2)+
    scale_color_manual(name = "",values = c(
      'Median' = 'forestgreen',
      'Current' = 'black')) +
    scale_fill_manual(name = paste0('Percentiles\n(',climatology_start_year,' - 2020)'), values = c("darkblue","cyan","green","orange", 'darkred'),
                      breaks = c("95th - Max", "75th - 95th", "25th - 75th", "5th - 25th", "Min - 5th")) +
    ylab("Snow Water Equivalent (mm)")+
    xlab("Date")+
    theme_bw(base_size = 20)+
    theme(plot.title = element_text(hjust = 0.5),
          legend.title.align=0.5)
  
  return(plot)
  # },
  # error = function(e){
  #   plot = ggplot()+
  #     ylab("Snow Water Equivalent (mm)")+
  #     xlab("Date")+
  #     theme_bw(base_size = 16)+
  #     geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)
  # })
  
}


# plot_snotel_mobile = function(current_data, climatology_data, names){
#   tryCatch({
#     plot = ggplot()+
#       ggtitle(paste0(names, "\n(", as.Date(current_data$Date[length(current_data$Date)]), ")"))+
#       geom_ribbon(data = climatology_data, aes(x = as.Date(WY_date), ymin = swe_quantiles_075, ymax = swe_quantiles_095, fill = "75th - 95th"), alpha = 0.25)+
#       geom_ribbon(data = climatology_data, aes(x = as.Date(WY_date), ymin = swe_quantiles_025, ymax = swe_quantiles_075, fill = "25th - 75th"), alpha = 0.25)+
#       geom_ribbon(data = climatology_data, aes(x = as.Date(WY_date), ymin = swe_quantiles_005, ymax = swe_quantiles_025, fill = "5th - 25th"), alpha = 0.25)+
#       geom_line(data = climatology_data, aes(x = as.Date(WY_date), y = median_swe_mm, color = "Median"), size = 0.75)+
#       geom_line(data = current_data, aes(x = as.Date(current_data$Date), y = current_data$SWE, color = "Current"), size = 2)+
#       scale_color_manual(name = "",values = c(
#         'Median' = 'darkgreen',
#         'Current' = 'black')) +
#       scale_fill_manual(name = 'Percentiles', values = c("blue","green","red"),
#                         breaks = c("75th - 95th", "25th - 75th", "5th - 25th")) +
#       ylab("Snow Water Equivalent (mm)")+
#       xlab(NULL)+
#       theme_bw(base_size = 16)+
#       theme(legend.position = c(0.86, 0.73),
#             plot.title = element_text(hjust = 0.5, size = 20),
#             legend.title=element_blank(),
#             axis.text=element_text(size=22))+
#       scale_x_date(date_breaks = "3 month", date_labels = "%m-%y")
#     
#     return(plot)
#   },
#   error = function(e){
#     plot = ggplot()+
#       ylab("Snow Water Equivalent (mm)")+
#       xlab("Date")+
#       theme_bw(base_size = 16)+
#       geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)
#   })
#   
# }
# 
# 
# 
# plot_snotel_precip = function(current_data, climatology_data, names){
#   tryCatch({
#     plot = ggplot()+
#       ggtitle(paste0(names, " (", as.Date(current_data$Date[length(current_data$Date)]), ")"))+
#       geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = precip_quantiles_075, ymax = precip_quantiles_095, fill = "75th - 95th"), alpha = 0.25)+
#       geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = precip_quantiles_025, ymax = precip_quantiles_075, fill = "25th - 75th"), alpha = 0.25)+
#       geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = precip_quantiles_005, ymax = precip_quantiles_025, fill = "5th - 25th"), alpha = 0.25)+
#       geom_line(data = climatology_data, aes(x = WY_date, y = median_precip_mm, color = "Median"), size = 0.75)+
#       geom_line(data = current_data, aes(x = current_data$Date, y = current_data$Precip, color = "Current"), size = 1.5)+
#       scale_color_manual(name = "" ,values = c(
#         'Median' = 'black',
#         'Current' = 'blue')) +
#       scale_fill_manual(name = 'Percentiles', values = c("blue","green","red"),
#                         breaks = c("75th - 95th", "25th - 75th", "5th - 25th")) +
#       ylab("Accumulated Precipitation (mm)")+
#       xlab("Date")+
#       theme_bw(base_size = 16)
# 
#     return(plot)
#   },
#   error = function(e){
#     plot = ggplot()+
#       ylab("Accumulated Precipitation (mm)")+
#       xlab("Date")+
#       theme_bw(base_size = 16)+
#       geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)
#   })
#   
# }
# 
# write.dir = paste0(export.dir, "snotel/plots/")
# 
error_plot = ggplot()+
  ylab("Snow Water Equivalent (mm)")+
  xlab("Date")+
  theme_bw(base_size = 16)+
  geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)


write.dir = '/home/zhoylman/mco-drought-indicators-data/snotel/plots/'

sites_of_interest = sites_with_climatology$site_id %>% unique()

print('starting plotting')

for(i in 1:length(sites_of_interest)){
  filename = paste0(write.dir,"snotel_plot_", sites_of_interest[i],".png")
  tryCatch({
    temp_plot = plot_snotel(current, sites_with_climatology, sites_of_interest[i], base_year)
    ggsave(filename, plot = temp_plot, units = c("in"), width = 9, height = 7, dpi = 150)
  },
  error = function(e){
    ggsave(filename, plot = error_plot, units = c("in"), width = 9, height = 7, dpi = 150)
  })
  print(i)
}

print('finished plotting')
print('done')

# 
# for(i in 1:length(snotel$site_name)){
#   filename = paste0(write.dir,"snotel_plot_mobile_", i,".png")
#   temp_plot = plot_snotel_mobile(current_select[[i]], climatology_WY[[i]], snotel$site_name[i])
#   tryCatch({
#     ggsave(filename, plot = temp_plot, units = c("in"), width = 6, height = 5, dpi = 150)
#   },
#   error = function(e){
#     ggsave(filename, plot = error_plot, units = c("in"), width = 7, height = 4, dpi = 150)
#   })
# }
# 
# for(i in 1:length(snotel$site_name)){
#   filename = paste0(write.dir,"precip_snotel_plot_", i,".png")
#   temp_plot = plot_snotel_precip(current_select[[i]], climatology_WY[[i]], snotel$site_name[i])
#   tryCatch({
#     ggsave(filename, plot = temp_plot, units = c("in"), width = 7, height = 4, dpi = 150)
#   },
#   error = function(e){
#     ggsave(filename, plot = error_plot, units = c("in"), width = 7, height = 4, dpi = 150)
#   })
# }
