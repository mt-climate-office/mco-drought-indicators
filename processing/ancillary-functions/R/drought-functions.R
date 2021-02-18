spi_fun = function(x) {
  #first try log logistic
  tryCatch(
    {
      x = as.numeric(x)
      #if precip is 0, replace it with 0.5%tile (really dry)
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

anomaly = function(x){
  x_mean = mean(x, na.rm = T)
  anomaly = ((x[length(x)])/x_mean)*100
  return(anomaly)
}

percentile_inverse = function(x){
  tryCatch({
    temp_cdf = ecdf(x) 
    cdf = temp_cdf(x)
    return(cdf[length(cdf)]*100)
  }, error = function(e) {
    return(NA)
  })
}

raw_amount = function(x){
  return(x[length(x)]/25.4)
}