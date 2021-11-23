#function to fit a gamma distrobuion to a vector of data
#export options (export_opts) allows the user to return 
#SPI valules if export_opts = 'SPI', CDF values if export_opts = 'CDF'
#or the gamma distrobution paramters if export_opts = 'params'.
#the function also allows the user to return either the latest
#CDF or SPI values when return_latest = T. when return_latest = F
#the entire SPI or CDF vector is returned. Default is to return latest. 

gamma_fit_spi = function(x, export_opts = 'SPI', return_latest = T) {
  #load the package needed for these computations
  library(lmomco)
  #first try gamma
  tryCatch(
    {
      x = as.numeric(x)
      #if precip is 0, replace it with 0.01mm Really Dry
      if(any(x == 0, na.rm = T)){
        index = which(x == 0)
        x[index] = 0.01
      }
      #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
      pwm = pwm.ub(x)
      #Probability-Weighted Moments to L-moments
      lmoments_x = pwm2lmom(pwm)
      #fit gamma
      fit.gam = pargam(lmoments_x)
      #compute probabilistic cdf 
      fit.cdf = cdfgam(x, fit.gam)
      #compute spi
      spi = qnorm(fit.cdf, mean = 0, sd = 1)
      if(return_latest == T){
        if(export_opts == 'CDF'){
          return(fit.cdf[length(fit.cdf)]) 
        }
        if(export_opts == 'params'){
          return(fit.gam) 
        }
        if(export_opts == 'SPI'){
          return(spi[length(spi)]) 
        }
      }
      if(return_latest == F){
        if(export_opts == 'CDF'){
          return(fit.cdf) 
        }
        if(export_opts == 'params'){
          return(fit.gam) 
        }
        if(export_opts == 'SPI'){
          return(spi) 
        }
      }
      
    },
    #else return NA
    error=function(cond) {
      return(NA)
    })
}

spi_fun = function(x) {
  #load the package needed for these computations
  library(lmomco)
  #first try gamma
  tryCatch(
    {
      x = as.numeric(x)
      #if precip is 0, replace it with 0.01mm Really Dry
      if(any(x == 0, na.rm = T)){
        index = which(x == 0)
        x[index] = 0.01
      }
      #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
      pwm = pwm.ub(x)
      #Probability-Weighted Moments to L-moments
      lmoments_x = pwm2lmom(pwm)
      #fit gamma
      fit.gam = pargam(lmoments_x)
      #compute probabilistic cdf 
      fit.cdf = cdfgam(x, fit.gam)
      #compute standard normal equivelant
      standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
      return(standard_norm[length(standard_norm)]) 
    },
    #else return NA
    error=function(cond) {
      return(NA)
    })
}

spei_fun = function(x) {
  #load the package needed for these computations
  library(lmomco)
  #first try log logistic
  tryCatch(
    {
      x = as.numeric(x)
      #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
      pwm = pwm.ub(x)
      #Probability-Weighted Moments to L-moments
      lmoments_x = pwm2lmom(pwm)
      #fit generalized logistic
      fit.parglo = parglo(lmoments_x)
      #compute probabilistic cdf 
      fit.cdf = cdfglo(x, fit.parglo)
      #compute standard normal equivelant
      standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
      return(standard_norm[length(standard_norm)])
    },
    #else return NA
    error=function(cond) {
      return(NA)
    })
}

glo_fit_spei = function(x, export_opts = 'SPEI', return_latest = T) {
  #load the package needed for these computations
  library(lmomco)
  #first try gamma
  tryCatch(
    {
      x = as.numeric(x)
      #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
      pwm = pwm.ub(x)
      #Probability-Weighted Moments to L-moments
      lmoments_x = pwm2lmom(pwm)
      #fit generalized logistic
      fit.parglo = parglo(lmoments_x)
      #compute probabilistic cdf 
      fit.cdf = cdfglo(x, fit.parglo)
      #compute spi
      spei = qnorm(fit.cdf, mean = 0, sd = 1)
      if(return_latest == T){
        if(export_opts == 'CDF'){
          return(fit.cdf[length(fit.cdf)]) 
        }
        if(export_opts == 'params'){
          return(fit.parglo) 
        }
        if(export_opts == 'SPEI'){
          return(spei[length(spei)]) 
        }
      }
      if(return_latest == F){
        if(export_opts == 'CDF'){
          return(fit.cdf) 
        }
        if(export_opts == 'params'){
          return(fit.parglo) 
        }
        if(export_opts == 'SPEI'){
          return(spei) 
        }
      }
      
    },
    #else return NA
    error=function(cond) {
      return(NA)
    })
}


eddi_fun = function(x) {
  #define coeffitients
  C0 = 2.515517
  C1 = 0.802853
  C2 = 0.010328
  d1 = 1.432788
  d2 = 0.189269
  d3 = 0.001308
  
  # following Hobbins et al., 2016
  x = as.numeric(x)
  
  if(all(is.na(x))){
    return(NA)
  } else {
    
    #Rank PET (1 = max)
    rank_1 = rank(-x)
    
    #Calcualte emperical probabilities
    prob = ((rank_1 - 0.33)/(length(rank_1) + 0.33))
    
    #compute W (whaterver that is)
    W = numeric(length(prob))
    for(i in 1: length(prob)){
      if(prob[i] <= 0.5){
        W[i] = sqrt(-2*log(prob[i]))
      } else {
        W[i] = sqrt(-2*log(1 - prob[i]))
      }
    }
    
    #Find indexes which need inverse EDDI sign
    reverse_index = which(prob > 0.5)
    
    #Compute EDDI
    EDDI = W - ((C0 + C1*W + C2*W^2)/(1 + d1*W + d2*W^2 + d3*W^3))
    
    #Reverse sign of EDDI values where prob > 0.5
    EDDI[reverse_index] = -EDDI[reverse_index]
    
    #Return Current Value
    return(EDDI[length(EDDI)])
  }
}

#percent of normal
percent_of_normal = function(x){
  x_mean = mean(x, na.rm = T)
  percent_of_normal = ((x[length(x)])/x_mean)*100
  return(percent_of_normal)
}

#deviation from normal
deviation_from_normal = function(x){
  x_mean = mean(x, na.rm = T)
  deviation_from_normal = ((x[length(x)]) - x_mean)
  return(deviation_from_normal)
}

percentile_inverse = function(x){
  tryCatch({
    #bins are based on emperical quantile (percentiles) based on vector
    bins = quantile(x, seq(0,1,by = 0.01))
    #convert the first bin to -Inf to catch the min values as 1st percentile
    bins[1] = -Inf
    #compute percentiles based on the bins
    percentiles = .bincode(x, bins)
    return(percentiles[length(percentiles)])
  }, error = function(e) {
    return(NA)
  })
}

raw_amount = function(x){
  return(x[length(x)]/25.4)
}

fdates  = function(filenames,dateformat=F,n=NULL){
  # this function extracts dates from filenames that contain a date in YYYYMMDD format. 
  # the date portion of the filename must be separated by underscores from the rest of the 
  # filename.  the function searches each filename for numeric elements with 8 digits.  
  # the filenane extension is automatically removed.  
  #
  # the 'filenames' argument is any vector of filenames
  # 'dateformat' causes the returned dates to be in R date format
  # 'n' overrides the automatic search and instead returns the nth element separated by underscores.
  if(length(filenames)==0) {cat("filenames supplied have length 0\n");return(NULL)}
  
  # strip extension ~~~~~~~~~~~~~~~~~~~~
  filenames <- sapply(strsplit(basename(filenames),"\\."), function(x) paste(x[1:(length(x)-1)],collapse="."))
  if(is.null(n)){
    dts<-sapply(strsplit(filenames,"_"),function(x) {y<-x[!is.na(suppressWarnings(as.numeric(x))) & nchar(x)==8];if(length(y)==1) y else NA})
  } else  dts<-sapply(strsplit(basename(filenames),"_"),function(x) x[n])
  if(dateformat) dts <- as.Date(dts,"%Y%m%d")
  dts
}