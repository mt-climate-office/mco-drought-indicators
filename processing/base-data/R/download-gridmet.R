#define git dir
git.dir = '~/mco-drought-indicators/'
export.dir = '~/mco-drought-indicators-data/'

# import ancillary functions
source(paste0(git.dir, '/processing/ancillary-functions/R/load-libs.R'))

get_gridment = function(varname, year, export.dir) {
  download.file(url = paste0('https://www.northwestknowledge.net/metdata/data/',
                             varname,
                             '_',
                             year,
                             '.nc'),
            destfile = paste0(export.dir,
                              'gridmet/',
                              varname,
                              '/',
                              varname,
                              '_',
                              year,
                              '.nc')
            )
}

vars = c('pr', 'pet', 'tmmx')
years = c(1991:2023)

for(v in 1:length(vars)[1]){
  for(y in years){
    get_gridment(vars[v], y, export.dir)
  }
}
