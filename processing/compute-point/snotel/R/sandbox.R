library(magrittr)
#library(tidyverse)
library(dplyr)
library(lmom)
library(snotelr)
library(ggplot2)
library(plotly)

calc_gam <-
  function(x){
    x[x<=0] <- 0.001
    tryCatch(
      suppressWarnings(
        x %>%
          lmom::samlmu() %>%
          lmom::pelgam()
      ),
      error = 
        function(e){
          as.numeric(NA)
        }
    )
  }

calc_p_swe <-
  function(target_gam, future_gam, p = 0.5){
    if(length(target_gam) != 2 | length(future_gam) != 2){
      return(as.numeric(NA))
    }
    1 - (
      lmom::quagam(f = p, para = target_gam) %>%
        lmom::cdfgam(para = future_gam)
    )
    
  }

my_quagam <-
  function(gam, p){
    if(length(gam) != 2){
      return(rep(as.numeric(NA), length(p)))
    }
    
    lmom::quagam(f = p, para = gam)
    
  }

calc_future_swe <-
  function(x){
    name <- 
      x$name[1]
    
    start_date <-
      max(x$date)
    
    start_swe_last <-
      x$swe[x$date == max(x$date)]
    
    x %<>%
      dplyr::mutate(
        name = name,
        snow_year = 
          ifelse(lubridate::month(date) > 9, 
                 lubridate::year(date) + 1, 
                 lubridate::year(date)))
    
    this_year <-
      dplyr::filter(x, snow_year == max(snow_year))
    
    past_years <-
      x %>%
      dplyr::filter(snow_year < max(snow_year),
                    snow_year >= (max(snow_year) - 30))
    
    out <-
      past_years %>%
      dplyr::mutate(
        start_date =
          dplyr::case_when(
            lubridate::month(date) == lubridate::month(start_date) &
              lubridate::day(date) == lubridate::day(start_date) ~
              date
          ),
        start_swe =
          dplyr::case_when(
            lubridate::month(date) == lubridate::month(start_date) &
              lubridate::day(date) == lubridate::day(start_date) ~
              swe
          )
      ) %>%
      dplyr::group_by(snow_year) %>%
      dplyr::mutate(start_swe = min(start_swe, na.rm = TRUE),
                    start_date = min(start_date, na.rm = TRUE),
                    delta_swe = swe - start_swe,
                    delta_time = date - start_date) %>%
      dplyr::ungroup() %>%
      na.omit() %>%
      dplyr::mutate(future_swe = pmax(start_swe_last + delta_swe, 0)) %>%
      dplyr::group_by(delta_time) %>%
      dplyr::summarise(
        climatology_ecdf =
          list(ecdf(swe)),
        future_gam = 
          list(
            calc_gam(future_swe)
          ),
        present_swe = tail(swe, 1)
      ) %>%
      dplyr::transmute(
        date = 
          lubridate::as_date(start_date) + delta_time,
        climatology_ecdf,
        future_gam,
        present_swe
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        future = list(
          my_quagam(
            future_gam,
            p = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
          )
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(future_gam = ifelse(date <= start_date, NA, future_gam),
                    future = ifelse(date <= start_date, list(rep(NA, 5)), future),
                    future = ifelse(date == start_date, list(rep(start_swe_last, 5)), future),
                    present_swe = ifelse(date > start_date, NA, present_swe),
                    name = name)
    
    normal_ecdf <-
      out$climatology_ecdf %>%
      purrr::map(~quantile(.x, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))) %>%
      do.call(rbind, .) %>%
      magrittr::set_colnames(
        c("1%", "5%", "25%", "50%", "75%", "95%", "99%")
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date = out$date) %>%
      dplyr::select(date, everything())
    
    future <-
      out$future %>%
      do.call(rbind, .) %>%
      magrittr::set_colnames(
        c("1%", "5%", "25%", "50%", "75%", "95%", "99%")
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date = out$date) %>%
      na.omit() %>%
      dplyr::select(date, everything())
    
    present <-
      this_year %>%
      dplyr::select(date, swe)
    
    return(
      list(
        name = name,
        present = present,
        future = future,
        climatology = normal_ecdf
      )
    )
    
  }

plot_future <-
  function(x){
    ggplot() +
      geom_ribbon(data = 
                    x$climatology,
                  mapping = aes(x = date, ymin = `1%`, ymax = `5%`,
                  fill = "Min - 5th"),
                  alpha = 0.2) +
      geom_ribbon(data = 
                    x$climatology,
                  mapping = aes(x = date, ymin = `5%`, ymax = `25%`,
                  fill = "5th - 25th"),
                  alpha = 0.2) +
      geom_ribbon(data = 
                    x$climatology,
                  mapping = aes(x = date, ymin = `25%`, ymax = `75%`,
                  fill = "25th - 75th"),
                  alpha = 0.2) +
      geom_ribbon(data = 
                    x$climatology,
                  mapping = aes(x = date, ymin = `75%`, ymax = `95%`,
                  fill = "75th - 95th"),
                  alpha = 0.2) +
      geom_ribbon(data = 
                    x$climatology,
                  mapping = aes(x = date, ymin = `95%`, ymax = `99%`,
                  fill = "95th - Max"),
                  alpha = 0.2) +
      geom_line(data = 
                  x$climatology,
                mapping = aes(x = date, y = `50%`,
                linetype = 'Current'), color = "forestgreen") +
      geom_line(data = 
                  x$climatology,
                mapping = aes(x = date, y = `1%`),
                color = "#94292E") +
      geom_line(data = 
                  x$climatology,
                mapping = aes(x = date, y = `99%`),
                color = "#2D2796") +
      geom_ribbon(data = 
                    x$future,
                  mapping = aes(x = date, ymin = `25%`, ymax = `75%`),
                  fill = "black",
                  alpha = 0.2) +
      # geom_ribbon(data = 
      #               x$future,
      #             mapping = aes(x = date, ymin = `5%`, ymax = `95%`),
      #             fill = "black",
      #             alpha = 0.1) +
      geom_line(data = 
                  x$future,
                mapping = aes(x = date, y = `50%`,
                linetype = 'Forecast', size = 'Forecast'), color = "black") +
      geom_line(data = 
                  x$present,
                mapping = aes(x = date, y = swe, size = 'Current'),
                color = "black") +
      scale_y_continuous(limits = c(0, NA),
                         expand = expansion(c(0,0.05),0),
                         name = "Snow Water Equivalent (mm)") +
      scale_x_date(limits = lubridate::as_date(c("2023-10-01", "2024-08-01")),
                   breaks = lubridate::as_date(c("2023-10-01", "2024-01-01", "2024-04-01", "2024-07-01", "2024-10-01")),
                   date_labels = "%B %Y",
                   expand = expansion(0,0),
                   name = NULL) +
      theme_bw()  +
      ggtitle(x$name)+
      theme(plot.title = element_text(hjust = 0.5, size=16))+
      scale_fill_manual(name = paste0('Observed\nPercentiles'), values = c("darkblue","cyan","green","orange", 'darkred'),
                        breaks = c("95th - Max", "75th - 95th", "25th - 75th", "5th - 25th", "Min - 5th")) +
      # scale_color_manual(name = "",values = c(
      #   'Median' = 'forestgreen',
      #   'Current' = 'black',
      #   'Most Likely' = 'black')) +
      scale_size_manual(NULL,values=c("Current"=1.5,"Forecast"=0.5))+
      scale_linetype_manual(NULL,values=c("Current"=1,"Forecast"=2))
      
  }

## Example for SNOTEL site 901 (Stuart Mountain)
stuart_mtn <-
  snotelr::snotel_download(901, 
                           internal = TRUE) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(name = site_name %>% stringr::str_to_title(),
                   date = lubridate::as_date(date),
                   swe = snow_water_equivalent)

plot = calc_future_swe(stuart_mtn) %>%
  plot_future()

plot

ggsave(plot, filename = '/home/zhoylman/temp/test.png', units = c("in"), width = 9, height = 7, dpi = 150)

sites_with_climatology = read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_climatology.csv')
sites_of_interest = sites_with_climatology$site_id %>% unique()


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
