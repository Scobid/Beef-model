# base prices
if (which(qtr == i) == 1) {
  
  calf_2["price steer", ii] <- round(fit(log_data(pp$calf$steer$data))$yhat[i], 2)
  calf_2["price heifer", ii] <- round(fit(log_data(pp$calf$heifer$data))$yhat[i], 2)
  
} 

# otherwise incorporate price transmission
if (which(qtr == i) == 1) {
  
  ## steer prices
  coef <- fit(log_data(pp$calf$steer$data))$coef     
  
  # price transmission effect
  lag_p <- calf_2["price steer", which(qtr==i)-1]
  
  # seasonal effect
  paste("x", ii, sep="") %in%names()
  
  # output price effect
  out_p <- 
    
    # trend effect
    
    calf_2["price steer", ii] <- exp(coef[1] + log(lag_p)*coef[7] + 
                                       
                                       rm(coef)
                                     
}