library(magrittr)
library(dplyr)
library(lmom)
library(snotelr)
library(ggplot2)
library(doParallel)

# Load all functions
list.files("/home/zhoylman/swe-futures/R/", full.names = TRUE) %>%
  purrr::walk(source)

## Example for SNOTEL site 901 (Stuart Mountain)
# plot = snotelr::snotel_download(901, 
#                          internal = TRUE) %>%
#   calc_future_swe() %>%
#   plot_future()
# 
# plot
# 
# ggsave(plot, filename = '/home/zhoylman/temp/test.png', units = c("in"), width = 9, height = 7, dpi = 150)

sites_with_climatology = readr::read_csv('/home/zhoylman/mco-drought-indicators-data/snotel/climatology/site_climatology.csv')
sites_of_interest = sites_with_climatology$site_id %>% unique()
write.dir = '/home/zhoylman/mco-drought-indicators-data/snotel/plots/'

errors = c()

cl= makeCluster(10)
registerDoParallel(cl)
foreach(i = 1:length(sites_of_interest),
        .packages = c('snotelr', 'magrittr', 'lmom', 'ggplot2', 'dplyr')) %dopar% {
  filename = paste0(write.dir,"snotel_plot_", sites_of_interest[i],".png")
  list.files("/home/zhoylman/swe-futures/R/", full.names = TRUE) %>%
    purrr::walk(source)

  tryCatch({
    ## Example for SNOTEL site 901 (Stuart Mountain)
    temp_plot = snotelr::snotel_download(sites_of_interest[i], 
                                         internal = TRUE) %>%
      calc_future_swe() %>%
      plot_snotel()
    
    ggsave(filename, plot = temp_plot, units = c("in"), width = 9, height = 7, dpi = 150)
  },
  error = function(e){
    print('error!!')
    errors = append(errors, i)
  })
  print(paste0(i, ' of ', length(sites_of_interest)))
}
stopCluster(cl)
