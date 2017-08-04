## STATIC SIMULATION MODEL 
## BASE MODEL
## SCENARIO: 
## ORIGINAL: TOR TOLHURST (OCT/2014)
## LAST UPDATE:


## CHOOSE FIRST QUARTER TO SHOCK
qtr <- c(3, 4, 1, 2) 
source("beef/code-version2/step-0 preliminaries.R")



for (i in qtr) {
  
  # column name equivalent of i (to run in different order and 1, 2, 3, 4)
  ii <- paste("Q", i, sep="")
  
  
  
  ###############################
  #### COW-CALF
  ###############################
  
  ## QUANTITIES
  
  # load total supply and demand, calculate interprovincial imports (mip) as residual
  calf_1["q_sup total", ii] <- round(fit(log_data(qq$calf$supply$data))$yhat[i])
  calf_1["q_dem total", ii] <- round(fit(log_data(qq$calf$demand$data))$yhat[i])
  calf_1["q_mip total", ii] <- calf_1["q_dem total", ii] - calf_1["q_sup total", ii]
  
  # supply and mip by gender (derived by assumption)
  calf_1["q_sup steer",  ii] <- round(calf_1["q_sup total", ii]*assume1$birth)
  calf_1["q_sup heifer", ii] <- round(calf_1["q_sup total", ii]*(1-assume1$birth)) 
  calf_1["q_mip steer",  ii] <- round(calf_1["q_mip total", ii]*assume1$mip$calf)
  calf_1["q_mip heifer", ii] <- round(calf_1["q_mip total", ii]*(1-assume1$mip$calf))
    
  # demand by gender (based on weighted ratio of gender in supply and mip)
  temp <- (calf_1["q_sup steer",  ii]+calf_1["q_mip steer",  ii])/(calf_1["q_sup total", ii]+calf_1["q_mip total", ii])
  calf_1["q_dem steer",  ii] <- round(calf_1["q_dem total", ii]*temp)
  calf_1["q_dem heifer", ii] <- round(calf_1["q_dem total", ii]*(1-temp))
  rm(temp)
  
  ## PRICES
  
  # load estimated prices
  calf_1["price steer", ii] <- round(fit(log_data(pp$calf$steer$data))$yhat[i], 2)
  calf_1["price heifer", ii] <- round(fit(log_data(pp$calf$heifer$data))$yhat[i], 2)
  
  
  
  
  
  ###############################
  #### BACKGROUNDING
  ###############################
  
  ## QUANTITIES
  
  # load provincial supply and demand
  bkgd_1["q_sup total", ii] <- round(fit(log_data(qq$bkgrd$supply$data))$yhat[i])
  bkgd_1["q_dem total", ii] <- round(fit(log_data(qq$bkgrd$demand$data))$yhat[i])
  
  # supply retained for replacement by gender
  # note: uses calf supply and mip from two quarters ago
  
  if (which(qtr == i) <= 2){
    
    tmp1 <- which(colnames(inventory1) == ii) - 2
    den1 <- inventory1["calf supply", tmp1] + inventory1["calf mip", tmp1] 
    num1 <- inventory1["calf supply", tmp1]*(assume1$birth) + inventory1["calf mip", tmp1]*(assume1$mip$calf) 
    if (num1 > den1) num1 <- den1
    num2 <- den1 - num1
    
  }
  
  if (which(qtr == i) > 2) {
    
    # gender ratio of calf supply and mip two quarters ago
    tmp1 <- paste("Q", qtr[which(qtr == i)-2], sep="")
    den1 <- calf_1["q_sup total",  tmp1] + calf_1["q_mip total",  tmp1]
    num1 <- calf_1["q_sup steer",  tmp1] + calf_1["q_mip steer",  tmp1]
    num2 <- calf_1["q_sup heifer", tmp1] + calf_1["q_mip heifer", tmp1]
    
  }
  
  # provincial supply by gender net of replacements
  bkgd_1["q_sup steer",  ii] <- round(bkgd_1["q_sup total", ii]*(1-assume1$replace$s)*(num1/den1))
  bkgd_1["q_rep steer",  ii] <- round(bkgd_1["q_sup total", ii]*(assume1$replace$s)*(num1/den1))
  bkgd_1["q_sup heifer", ii] <- round(bkgd_1["q_sup total", ii]*(1-assume1$replace$h)*(num2/den1))
  bkgd_1["q_rep heifer", ii] <- round(bkgd_1["q_sup total", ii]*(assume1$replace$h)*(num2/den1))
  bkgd_1["q_rep total",  ii] <- bkgd_1["q_rep steer",  ii]+bkgd_1["q_rep heifer", ii]
  
  # remove temporary variables
  rm(tmp1, den1, num1, num2)
  
    
  # demand by gender -- determined by ratio of heifers to steers in provnicial finishing supply
  fnsh_1["q_sup steer",  ii] <- round(fit(log_data(qq$fnshg$steer$supply$data))$yhat[i])
  fnsh_1["q_sup heifer", ii] <- round(fit(log_data(qq$fnshg$heifer$supply$data))$yhat[i])
  fnsh_1["q_sup total",  ii] <- fnsh_1["q_sup steer",  ii]+fnsh_1["q_sup heifer",  ii]
  bkgd_1["q_dem steer",  ii] <- round(bkgd_1["q_dem total",  ii]*(fnsh_1["q_sup steer",  ii]/fnsh_1["q_sup total",  ii]))
  bkgd_1["q_dem heifer", ii] <- round(bkgd_1["q_dem total",  ii]*(fnsh_1["q_sup heifer", ii]/fnsh_1["q_sup total",  ii]))
  
  # feeder exports to U.S. by gender
  bkgd_1["q_xus total",  ii] <- round(fit(qq$bkgrd$export$data, logeq = F)$yhat)[i]
  bkgd_1["q_xus steer",  ii] <- round(bkgd_1["q_xus total",  ii]*assume1$exportfdr)
  bkgd_1["q_xus heifer", ii] <- round(bkgd_1["q_xus total",  ii]*(1-assume1$exportfdr))
  
  # interprovincial imports includes supply from dairy operations
  # temp = dairy feeders (irrespective of gender)
  temp <- round(fit(qq$bkgrd$dairy$data, logeq = F)$yhat)[i]
  
  bkgd_1["q_mip steer",  ii] <- bkgd_1["q_dem steer",   ii] + bkgd_1["q_xus steer",   ii] - bkgd_1["q_sup steer",   ii] - round(temp*assume1$dairy_fdr)
  bkgd_1["q_mip heifer", ii] <- bkgd_1["q_dem heifer",  ii] + bkgd_1["q_xus heifer",  ii] - bkgd_1["q_sup heifer",  ii] - round(temp*(1-assume1$dairy_fdr))
  bkgd_1["q_mip total",  ii] <- bkgd_1["q_mip steer",   ii] + bkgd_1["q_mip heifer",  ii]
  rm (temp)
  
  
  ## PRICES
  bkgd_1["price steer",  ii] <- round(fit(log_data(pp$bkgrd$steer$data))$yhat[i], 2)
  bkgd_1["price heifer", ii] <- round(fit(log_data(pp$bkgrd$heifer$data))$yhat[i], 2)
  
  
  
  
  ###############################
  #### FINISHING
  ###############################
  
  ## QUANTITIES

  # load supply and demand by gender
  fnsh_1["q_sup steer",  ii] <- round(fit(log_data(qq$fnshg$steer$supply$data))$yhat[i])
  fnsh_1["q_sup heifer", ii] <- round(fit(log_data(qq$fnshg$heifer$supply$data))$yhat[i])
  fnsh_1["q_sup total",  ii] <- fnsh_1["q_sup steer",  ii]+fnsh_1["q_sup heifer",  ii]
  fnsh_1["q_dem steer",  ii] <- round(fit(log_data(qq$fnshg$steer$demand$data))$yhat[i])
  fnsh_1["q_dem heifer", ii] <- round(fit(log_data(qq$fnshg$heifer$demand$data))$yhat[i])
  fnsh_1["q_dem total",  ii] <- fnsh_1["q_dem steer",  ii]+fnsh_1["q_dem heifer",  ii]
  
  # load exports to u.s. by gender
  fnsh_1["q_xus steer",  ii] <- round(fit(qq$fnshg$steer$export$data, logeq = F)$yhat[i])
  fnsh_1["q_xus heifer", ii] <- round(fit(qq$fnshg$heifer$export$data, logeq = F)$yhat[i])
  fnsh_1["q_xus total",  ii] <- fnsh_1["q_xus steer",  ii]+fnsh_1["q_xus heifer",  ii]
  
  # calculate interprovincial imports (mip) as residual
  fnsh_1["q_mip steer",  ii] <- fnsh_1["q_dem steer",   ii] + fnsh_1["q_xus steer",   ii] - fnsh_1["q_sup steer",   ii]
  fnsh_1["q_mip heifer", ii] <- fnsh_1["q_dem heifer",  ii] + fnsh_1["q_xus heifer",  ii] - fnsh_1["q_sup heifer",  ii]
  fnsh_1["q_mip total",  ii] <- fnsh_1["q_mip steer",   ii] + fnsh_1["q_mip heifer",  ii]
  
  
  
  ## PRICES
  
  # load estimated prices
  fnsh_1["price steer", ii] <- round(fit(log_data(pp$fnshg$steer$data))$yhat[i], 2)
  fnsh_1["price heifer", ii] <- round(fit(log_data(pp$fnshg$heifer$data))$yhat[i], 2)
  
  
  
  
  
  ###############################
  #### CULLED 
  ###############################
  
  ## QUANTITIES
  
  # residual slaughter capacity (in head per quarter)
  spare_capacity1 <- max(assume1$capacity - fnsh_1["q_sup total", ii], 0)
  
  # load demand and exports to us
  cull_1["q_dem bull",  ii] <- round(fit(log_data(qq$culld$bulls$demand$data))$yhat[i])
  cull_1["q_dem cow",   ii] <- round(fit(log_data(qq$culld$cows$demand$data))$yhat[i])
  cull_1["q_dem total", ii] <- cull_1["q_dem bull",  ii]+cull_1["q_dem cow", ii]
  cull_1["q_xus bull",  ii] <- round(fit(qq$culld$bulls$export$data, logeq = F)$yhat[i])
  cull_1["q_xus cow",   ii] <- round(fit(qq$culld$cows$export$data, logeq = F)$yhat[i])
  cull_1["q_xus total", ii] <- cull_1["q_xus bull",  ii]+cull_1["q_xus cow", ii]
  
  # load non-fed cow and bull inventory
  cull_1["q_inv bull",   ii] <- inventory1["cull bull", ii]
  cull_1["q_inv cow",    ii] <- inventory1["cull cow",  ii]
  cull_1["q_inv total",  ii] <- cull_1["q_inv bull",  ii] + cull_1["q_inv cow",  ii]
  
  # apply slaughter capacity constraint to demand (seperately for each gender)
  temp <- cull_1["q_dem bull",  ii]/cull_1["q_dem total",  ii]
  
  if (cull_1["q_dem bull",  ii] > temp*spare_capacity1) {
    
    cull_1["q_dem bull",  ii] <- temp*spare_capacity1
    cull_1["capacity constraint", ii] <- 1
    
  }
  
  if (cull_1["q_dem cow",   ii] > (1-temp)*spare_capacity1) {
    
    cull_1["q_dem cow",   ii] <- (1-temp)*spare_capacity1
    cull_1["capacity constraint", ii] <- 1
    
  } 
  
  cull_1["q_dem total", ii] <- cull_1["q_dem bull",  ii]+cull_1["q_dem cow", ii]
  
  
  # supply requires change (D = delta) in inventory 
  temp   <- which(colnames(inventory1) == ii)
  Dinv_b <- inventory1["cull bull", temp] - inventory1["cull bull", temp-1]
  Dinv_c <- inventory1["cull cow",  temp] - inventory1["cull cow",  temp-1]
  
  # derive provincial supply
  cull_1["q_sup bull",  ii] <- cull_1["q_dem bull", ii] + cull_1["q_xus bull", ii] + Dinv_b
  cull_1["q_sup cow",   ii] <- cull_1["q_dem cow",  ii] + cull_1["q_xus cow",  ii] + Dinv_c 
  cull_1["q_sup total", ii] <- cull_1["q_sup bull", ii] + cull_1["q_sup cow",  ii]
  
  
  # remove temporary inventory variables
  rm(Dinv_b, Dinv_c, temp)
  
  
  ## PRICES
  cull_1["price cow",  ii] <- round(fit(log_data(pp$culld$heifer$data))$yhat[i], 2)
  cull_1["price bull", ii] <- round(fit(log_data(pp$culld$steer$data))$yhat[i], 2)
  
  
  
  
  
  ###############################
  #### END CONSUMER MARKETS
  ###############################
  
  ## QUANTITIES
  
  # load provincial demand in lbs.
  proc_1["q_dem total", ii] <- round(fit(log_data(qq$markt$demand$data))$yhat[i]*assume1$popn[i])
  
  # provincial supply in lbs.
  proc_1["q_sup total", ii] <- fnsh_1["q_sup heifer", ii]*weight[1, ii]
  proc_1["q_sup total", ii] <- fnsh_1["q_sup steer", ii]*weight[2, ii] + proc_1["q_sup total", ii]
  proc_1["q_sup total", ii] <- cull_1["q_sup cow", ii]*weight[3, ii] + proc_1["q_sup total", ii]
  proc_1["q_sup total", ii] <- round(cull_1["q_sup bull", ii]*weight[4, ii] + proc_1["q_sup total", ii])
  
  # exports and imports 
  proc_1["q_xus total", ii] <- round(fit(qq$markt$export$usa$data, logeq = F)$yhat[i])
  proc_1["q_xrw total", ii] <- round(fit(qq$markt$export$row$data, logeq = T)$yhat[i])
  proc_1["q_mus total", ii] <- round(fit(qq$markt$import$usa$data, logeq = F)$yhat[i])
  proc_1["q_mrw total", ii] <- round(fit(qq$markt$import$row$data, logeq = F)$yhat[i])
  
  # interprovincial imports
  proc_1["q_mip total", ii] <- proc_1["q_dem total", ii] - proc_1["q_sup total", ii] + (proc_1["q_xus total", ii] + proc_1["q_xrw total", ii]) - (proc_1["q_mus total", ii] + proc_1["q_mrw total", ii])
  
  
  ## PRICES
  proc_1["price retail", ii] <- round(fit(log_data(pp$markt$retail$data))$yhat[i], 2)  
  proc_1["price wholes", ii] <- round(fit(log_data(pp$markt$wholesale$data))$yhat[i], 2)  
  
}



