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
  library(lubridate)
  #rule for start date depending on if date is between Oct - Dec use current year data,
  #else (Jan - Sept), start date is the previous year (water year)
  if(month(as.Date(Sys.time()))>=10){
    start = paste0(year(as.Date(Sys.time())),"-10-01")
  } else {
    start = paste0((year(as.Date(Sys.time()))-1),"-10-01")
  }
  
  tryCatch({
    #pull in data
    grabNRCS.data("SNTL", as.numeric(snotel$site_num[i]), timescale = "daily", DayBgn = start,
                  DayEnd = as.Date(Sys.time()))
  }, error = function(e){
    return(NA)
  }
  )
}

extract_columns <- function(data, collumn_name) {
  extracted_data <- data %>%
    select_(.dots = collumn_name)
  return(extracted_data)
}

clusterExport(cl, "extract_columns")

current_select = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(dplyr)
  collumn_name = c("Snow.Water.Equivalent..in..Start.of.Day.Values", 
                   "Precipitation.Accumulation..in..Start.of.Day.Values", "Date")
  tryCatch({
    extract_columns(current[[i]], collumn_name)
  }, error = function(e){
    return(NA)
  }
  )
}

stopCluster(cl)

toc()

#clean up data
for(i in 1:length(snotel$site_num)){
  if(length(current_select[[i]])>1){
    colnames(current_select[[i]]) = c("SWE", "Precip","Date")
    current_select[[i]]$yday = yday(current_select[[i]]$Date)
    current_select[[i]]$Date = as.POSIXct(current_select[[i]]$Date, format = "%Y-%m-%d")
    current_select[[i]]$SWE = current_select[[i]]$SWE*25.4
    current_select[[i]]$Precip = current_select[[i]]$Precip*25.4
  }
}

#load in climatology data
load(paste0(export.dir, "snotel/climatology/snotel_climatology.RData"))

climatology_WY = climatology

#compute index and sequences for water year and clean up data
for(i in 1:length(snotel$site_num)){
  if(length(climatology_WY[[i]]$yday) == 366){
    if(length(current_select[[i]])>1){
      climatology_WY[[i]] = climatology_WY[[i]] %>%
      dplyr::mutate(WY = c(seq(91,366,1), seq(1,90,1)))%>%
      dplyr::mutate(WY_date = as.POSIXct(as.Date(WY, origin = as.Date(current_select[[i]]$Date[1])), format = "%Y-%m-%d"))%>%
      dplyr::mutate(median_swe_mm = median_swe *25.4)%>%
      dplyr::mutate(swe_quantiles_005 = swe_quantiles_005 *25.4)%>%
      dplyr::mutate(swe_quantiles_025 = swe_quantiles_025 *25.4)%>%
      dplyr::mutate(swe_quantiles_075 = swe_quantiles_075 *25.4)%>%
      dplyr::mutate(swe_quantiles_095 = swe_quantiles_095 *25.4)%>%
      dplyr::mutate(median_precip_mm = median_precip *25.4)%>%
      dplyr::mutate(precip_quantiles_005 = precip_quantiles_005 *25.4)%>%
      dplyr::mutate(precip_quantiles_025 = precip_quantiles_025 *25.4)%>%
      dplyr::mutate(precip_quantiles_075 = precip_quantiles_075 *25.4)%>%
      dplyr::mutate(precip_quantiles_095 = precip_quantiles_095 *25.4)
    }
  }
    if(length(climatology_WY[[i]]$yday) == 365){
      if(length(current_select[[i]])>1){
        climatology_WY[[i]] = climatology_WY[[i]] %>%
          dplyr::mutate(WY = c(seq(91,365,1), seq(1,90,1)))%>%
          dplyr::mutate(WY_date = as.POSIXct(as.Date(WY, origin = as.Date(current_select[[i]]$Date[1])), format = "%Y-%m-%d"))%>%
          dplyr::mutate(median_swe_mm = median_swe *25.4)%>%
          dplyr::mutate(swe_quantiles_005 = swe_quantiles_005 *25.4)%>%
          dplyr::mutate(swe_quantiles_025 = swe_quantiles_025 *25.4)%>%
          dplyr::mutate(swe_quantiles_075 = swe_quantiles_075 *25.4)%>%
          dplyr::mutate(swe_quantiles_095 = swe_quantiles_095 *25.4)%>%
          dplyr::mutate(median_precip_mm = median_precip *25.4)%>%
          dplyr::mutate(precip_quantiles_005 = precip_quantiles_005 *25.4)%>%
          dplyr::mutate(precip_quantiles_025 = precip_quantiles_025 *25.4)%>%
          dplyr::mutate(precip_quantiles_075 = precip_quantiles_075 *25.4)%>%
          dplyr::mutate(precip_quantiles_095 = precip_quantiles_095 *25.4)
    }
  }
}

#remove 0's at end of time series - not sure why this is in my climatology

for(i in 1:length(snotel$site_name)){
  tryCatch({
    index = which(climatology_WY[[i]]$yday > 200 & climatology_WY[[i]]$median_precip_mm < 1)
    climatology_WY[[i]]$median_precip_mm[index] = NA
  }, error = function(e){
    return(NA)
  })
}


plot_snotel = function(current_data, climatology_data, names){
  tryCatch({
    plot = ggplot()+
      ggtitle(paste0(names, " (", as.Date(current_data$Date[length(current_data$Date)]), ")"))+
      geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = swe_quantiles_075, ymax = swe_quantiles_095, fill = "75th - 95th"), alpha = 0.25)+
      geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = swe_quantiles_025, ymax = swe_quantiles_075, fill = "25th - 75th"), alpha = 0.25)+
      geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = swe_quantiles_005, ymax = swe_quantiles_025, fill = "5th - 25th"), alpha = 0.25)+
      geom_line(data = climatology_data, aes(x = WY_date, y = median_swe_mm, color = "Median"), size = 0.75)+
      geom_line(data = current_data, aes(x = current_data$Date, y = current_data$SWE, color = "Current"), size = 1.5)+
      scale_color_manual(name = "",values = c(
        'Median' = 'black',
        'Current' = 'blue')) +
      scale_fill_manual(name = 'Percentiles', values = c("green","red","blue"),
                        breaks = c("75th - 95th", "25th - 75th", "5th - 25th")) +
      ylab("Snow Water Equivalent (mm)")+
      xlab("Date")+
      theme_bw(base_size = 16)
    
    return(plot)
  },
  error = function(e){
    plot = ggplot()+
      ylab("Snow Water Equivalent (mm)")+
      xlab("Date")+
      theme_bw(base_size = 16)+
      geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)
  })
  
}


plot_snotel_mobile = function(current_data, climatology_data, names){
  tryCatch({
    plot = ggplot()+
      ggtitle(paste0(names, "\n(", as.Date(current_data$Date[length(current_data$Date)]), ")"))+
      geom_ribbon(data = climatology_data, aes(x = as.Date(WY_date), ymin = swe_quantiles_075, ymax = swe_quantiles_095, fill = "75th - 95th"), alpha = 0.25)+
      geom_ribbon(data = climatology_data, aes(x = as.Date(WY_date), ymin = swe_quantiles_025, ymax = swe_quantiles_075, fill = "25th - 75th"), alpha = 0.25)+
      geom_ribbon(data = climatology_data, aes(x = as.Date(WY_date), ymin = swe_quantiles_005, ymax = swe_quantiles_025, fill = "5th - 25th"), alpha = 0.25)+
      geom_line(data = climatology_data, aes(x = as.Date(WY_date), y = median_swe_mm, color = "Median"), size = 0.75)+
      geom_line(data = current_data, aes(x = as.Date(current_data$Date), y = current_data$SWE, color = "Current"), size = 2)+
      scale_color_manual(name = "",values = c(
        'Median' = 'darkgreen',
        'Current' = 'black')) +
      scale_fill_manual(name = 'Percentiles', values = c("green","red","blue"),
                        breaks = c("75th - 95th", "25th - 75th", "5th - 25th")) +
      ylab("Snow Water Equivalent (mm)")+
      xlab(NULL)+
      theme_bw(base_size = 16)+
      theme(legend.position = c(0.86, 0.73),
            plot.title = element_text(hjust = 0.5, size = 20),
            legend.title=element_blank(),
            axis.text=element_text(size=22))+
      scale_x_date(date_breaks = "3 month", date_labels = "%m-%y")
    
    return(plot)
  },
  error = function(e){
    plot = ggplot()+
      ylab("Snow Water Equivalent (mm)")+
      xlab("Date")+
      theme_bw(base_size = 16)+
      geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)
  })
  
}



plot_snotel_precip = function(current_data, climatology_data, names){
  tryCatch({
    plot = ggplot()+
      ggtitle(paste0(names, " (", as.Date(current_data$Date[length(current_data$Date)]), ")"))+
      geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = precip_quantiles_075, ymax = precip_quantiles_095, fill = "75th - 95th"), alpha = 0.25)+
      geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = precip_quantiles_025, ymax = precip_quantiles_075, fill = "25th - 75th"), alpha = 0.25)+
      geom_ribbon(data = climatology_data, aes(x = WY_date, ymin = precip_quantiles_005, ymax = precip_quantiles_025, fill = "5th - 25th"), alpha = 0.25)+
      geom_line(data = climatology_data, aes(x = WY_date, y = median_precip_mm, color = "Median"), size = 0.75)+
      geom_line(data = current_data, aes(x = current_data$Date, y = current_data$Precip, color = "Current"), size = 1.5)+
      scale_color_manual(name = "" ,values = c(
        'Median' = 'black',
        'Current' = 'blue')) +
      scale_fill_manual(name = 'Percentiles', values = c("green","red","blue"),
                        breaks = c("75th - 95th", "25th - 75th", "5th - 25th")) +
      ylab("Accumulated Precipitation (mm)")+
      xlab("Date")+
      theme_bw(base_size = 16)

    return(plot)
  },
  error = function(e){
    plot = ggplot()+
      ylab("Accumulated Precipitation (mm)")+
      xlab("Date")+
      theme_bw(base_size = 16)+
      geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)
  })
  
}

write.dir = paste0(export.dir, "snotel/plots/")

error_plot = ggplot()+
  ylab("Snow Water Equivalent (mm)")+
  xlab("Date")+
  theme_bw(base_size = 16)+
  geom_text(label = "Sorry, no data available", aes(x = 1, y = 1), size = 9)

for(i in 1:length(snotel$site_name)){
  filename = paste0(write.dir,"snotel_plot_", i,".png")
  temp_plot = plot_snotel(current_select[[i]], climatology_WY[[i]], snotel$site_name[i])
  tryCatch({
    ggsave(filename, plot = temp_plot, units = c("in"), width = 7, height = 4, dpi = 150)
  },
  error = function(e){
    ggsave(filename, plot = error_plot, units = c("in"), width = 7, height = 4, dpi = 150)
  })
}

for(i in 1:length(snotel$site_name)){
  filename = paste0(write.dir,"snotel_plot_mobile_", i,".png")
  temp_plot = plot_snotel_mobile(current_select[[i]], climatology_WY[[i]], snotel$site_name[i])
  tryCatch({
    ggsave(filename, plot = temp_plot, units = c("in"), width = 6, height = 5, dpi = 150)
  },
  error = function(e){
    ggsave(filename, plot = error_plot, units = c("in"), width = 7, height = 4, dpi = 150)
  })
}

for(i in 1:length(snotel$site_name)){
  filename = paste0(write.dir,"precip_snotel_plot_", i,".png")
  temp_plot = plot_snotel_precip(current_select[[i]], climatology_WY[[i]], snotel$site_name[i])
  tryCatch({
    ggsave(filename, plot = temp_plot, units = c("in"), width = 7, height = 4, dpi = 150)
  },
  error = function(e){
    ggsave(filename, plot = error_plot, units = c("in"), width = 7, height = 4, dpi = 150)
  })
}
