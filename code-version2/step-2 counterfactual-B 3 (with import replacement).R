## STATIC SIMULATION MODEL 
## FOOT & MOUTH DISEASE SIMULATION
## SCENARIO: FDM IN GREY AND BRUCE COUNTIES
## FEDERAL AND PROVINCIAL BORDERS SHUT DOWN
## NO FEEDER MOVEMENT INTO THESE COUNTIES (25% of movement)
## CULL CATTLE (EQUIVALENT T0 10% OF CATTLE IN THESE TWO COUNTIES)
## % OF PROV. CATTLE: 1.6% CALVES, 2.7% STOCKERS, 1.6% NON-FED, 3.4% FINISHING
## ORIGINAL: TOR TOLHURST (NOV/2014)
## LAST UPDATE:

## CHOOSE FIRST QUARTER TO SHOCK
qtr <- c(3, 4, 1, 2)
source("beef/code-version2/step-1 base_model.R")


## CHANGE ANY UNDERLYING ASSUMPTIONS
assume2 <- assume1

## ADD "STOMPING OUT" CULL AMOUNTS
assume2$stomp$calf <- 0.016 # cull 10% of calves in bruce & grey county (=1.6% prov'l)
assume2$stomp$bkgd <- 0.027 # cull 10% of backgrounded stocker cattle in bruce & grey county
assume2$stomp$cull <- 0.016 # cull 10% of non-fed cattle in bruce & grey county
assume2$stomp$fnsh <- 0.034 # cull 10% of fed cattle in bruce & grey county

for (i in qtr) {

  ## reference indexes to run in different order than 1, 2, 3, 4
  # column name equivalent of i
  ii <- paste("Q", i, sep="")

  # number of iterations
  it <- which(qtr == i)


  ###############################
  #### COW-CALF
  ###############################

  ## BASE QUANTITY


  # first iteration: load total supply and demand
  if (it == 1){

    calf_2["q_sup total", ii] <- round(fit(log_data(qq$calf$supply$data))$yhat[i])
    calf_2["q_dem total", ii] <- round(fit(log_data(qq$calf$demand$data))$yhat[i])


    ## SHOCK: INCORPORATE STOMP OUT
    calf_2["q_sup total", ii] <- round((1-assume2$stomp$calf)*calf_2["q_sup total", ii])
    calf_2["q_dem total", ii] <- round((1-assume2$stomp$calf)*calf_2["q_dem total", ii])

  }

  # subsequent iterations # load base supply and demand AND incorporate effects of
  # changes in previous iteration into base supply and demand using coefficients
  if (it >= 2){

    # supply (note: Rail.Grade.Price..t.4 and CC.Inventory..t.4 effects won't change within four quarters)
    delta_q <- (calf_2["q_sup total", it-1]-calf_1["q_sup total", it-1])/calf_1["q_sup total", it-1]
    coef    <- fit(log_data(qq$calf$supply$data))$coef["xQuantity.Supplied..t.1."]
    calf_2["q_sup total", ii] <- round(fit(log_data(qq$calf$supply$data))$yhat[i]*(1+delta_q*coef))
    rm(delta_q, coef)

    # demand (note: how to do? xRail.Grade.Calf.Price....100.lbs.. xRail.Grade.Stocker.Price.....100.lbs.. )
    delta_q <- (calf_2["q_dem total", it-1]-calf_1["q_dem total", it-1])/calf_1["q_dem total", it-1]
    coef    <- fit(log_data(qq$calf$demand$data))$coef["xDemand..t.1."]
    calf_2["q_dem total", ii] <- round(fit(log_data(qq$calf$demand$data))$yhat[i]*(1+delta_q*coef))
    rm(delta_q, coef)

  }

  ## DERIVED QUANTITIES

  # calculate interprovincial imports (mip) as residual of supply and demand
  calf_2["q_mip total", ii] <- 0 #calf_1["q_dem total", ii] - calf_1["q_sup total", ii]

  ## SHOCK: NO INTERPROVINCIAL IMPORTS
  # therefore, demand goes down to meet supply (or vice versa)
  calf_2["q_dem total", ii] <- min(calf_2["q_dem total", ii], calf_2["q_sup total", ii])
  calf_2["q_sup total", ii] <- min(calf_2["q_dem total", ii], calf_2["q_sup total", ii])

  # supply by gender (derived by assumption)
  calf_2["q_sup steer",  ii] <- round(calf_2["q_sup total", ii]*assume2$birth)
  calf_2["q_sup heifer", ii] <- round(calf_2["q_sup total", ii]*(1-assume2$birth))
  calf_2["q_mip steer",  ii] <- round(calf_2["q_mip total", ii]*assume2$mip$calf)
  calf_2["q_mip heifer", ii] <- round(calf_2["q_mip total", ii]*(1-assume2$mip$calf))

  # demand by gender (based on weighted ratio of gender in supply and mip)
  temp <- (calf_2["q_sup steer",  ii]+calf_2["q_mip steer",  ii])/(calf_2["q_sup total", ii]+calf_2["q_mip total", ii])
  calf_2["q_dem steer",  ii] <- round(calf_2["q_dem total", ii]*temp)
  calf_2["q_dem heifer", ii] <- round(calf_2["q_dem total", ii]*(1-temp))


  ## remove redundant temporary variables
  rm(temp) # check cbind(calf_2[,it], calf_1[, it])




  ###############################
  #### BACKGROUNDING
  ###############################

  ## BASE QUANTITIES

  # first iteration: load total supply and demand
  if (it == 1){

    bkgd_2["q_sup total", ii] <- round(fit(log_data(qq$bkgrd$supply$data))$yhat[i])
    bkgd_2["q_dem total", ii] <- round(fit(log_data(qq$bkgrd$demand$data))$yhat[i])


    ## SHOCK: INCORPORATE STOMP OUT
    bkgd_2["q_sup total", ii] <- round((1-assume2$stomp$bkgd)*bkgd_2["q_sup total", ii])
    bkgd_2["q_dem total", ii] <- round((1-assume2$stomp$bkgd)*bkgd_2["q_dem total", ii])

  }

  # subsequent iterations
  if (it >= 2){

    # supply (note: no effect on xStocker.Bob.Calf.Price.Ratio..t.4. and xDairy.Feeder..t.1.)
    # the data in backgrounding supply is exogenously a function of cow-calf demand
    if (it > 2) {

      ## % change in cow-calf demand two quarters ago
      delta_q <- (calf_2["q_dem total", it-2]-calf_1["q_dem total", it-2])/calf_1["q_dem total", it]
      coef    <- 1 ## assume this coefficient equals one
      bkgd_2["q_sup total", ii] <- round(fit(log_data(qq$bkgrd$supply$data))$yhat[i]*(1+delta_q*coef))
      rm(delta_q, coef)
    }

    # for quarter 2
    if (it == 2) bkgd_2["q_sup total", ii] <- round(fit(log_data(qq$bkgrd$supply$data))$yhat[i])

    # demand
    delta_q <- (bkgd_2["q_sup total", it-1]-bkgd_1["q_sup total", it-1])/bkgd_1["q_sup total", it-1]
    coef    <- fit(log_data(qq$bkgrd$demand$data))$coef["xSupply..t.1."]
    bkgd_2["q_dem total", ii] <- round(fit(log_data(qq$bkgrd$demand$data))$yhat[i]*(1+delta_q*coef))

    # two-period lag price effect on demand
    if (it > 2){

      delta_q1   <- (mean(fnsh_2[1:2, it-2]) - mean(fnsh_1[1:2, it-2]))/mean(fnsh_1[1:2, it-2])
      coef1      <- fit(log_data(qq$bkgrd$demand$data))$coef["xAvg..Rail.Grade.Feeder.Price....100.lbs.....t.2."]
      delta_q2   <- (mean(calf_2[1:2, it-2]) - mean(calf_1[1:2, it-2]))/mean(calf_1[1:2, it-2])
      coef2      <- fit(log_data(qq$bkgrd$demand$data))$coef["xAvg..Rail.Grade.Calf.Price....100.lbs....t.2."]
      net_effect <- (1+delta_q*coef)*(1+delta_q1*coef1)*(1+delta_q2*coef2)
      bkgd_2["q_dem total", ii] <- round(net_effect*fit(log_data(qq$bkgrd$demand$data))$yhat[i])
      rm(delta_q1, coef1, delta_q2, coef2, net_effect)

    }

    rm(delta_q, coef)

  }


  ## DERIVED QUANTITIES

  # supply retained for replacement by gender
  # note: uses calf supply and mip from two quarters ago

  if (which(qtr == i) <= 2){

    tmp1 <- which(colnames(inventory1) == ii) - 2
    den1 <- inventory1["calf supply", tmp1] + inventory1["calf mip", tmp1]
    num1 <- inventory1["calf supply", tmp1]*(assume2$birth) + inventory1["calf mip", tmp1]*(assume2$mip$calf)
    if (num1 > den1) num1 <- den1
    num2 <- den1 - num1

  }

  if (which(qtr == i) > 2) {

    # gender ratio of calf supply and mip two quarters ago
    tmp1 <- paste("Q", qtr[which(qtr == i)-2], sep="")
    den1 <- calf_2["q_sup total",  tmp1] + calf_2["q_mip total",  tmp1]
    num1 <- calf_2["q_sup steer",  tmp1] + calf_2["q_mip steer",  tmp1]
    num2 <- calf_2["q_sup heifer", tmp1] + calf_2["q_mip heifer", tmp1]

  }


  # provincial supply by gender net of replacements
  bkgd_2["q_sup steer",  ii] <- round(bkgd_2["q_sup total", ii]*(1-assume2$replace$s)*(num1/den1))
  bkgd_2["q_rep steer",  ii] <- max(round(bkgd_2["q_sup total", ii]*(assume2$replace$s)*(num1/den1)), 0)
  bkgd_2["q_sup heifer", ii] <- round(bkgd_2["q_sup total", ii]*(1-assume2$replace$h)*(num2/den1))
  bkgd_2["q_rep heifer", ii] <- max(round(bkgd_2["q_sup total", ii]*(assume2$replace$h)*(num2/den1)), 0)
  bkgd_2["q_rep total",  ii] <- bkgd_2["q_rep steer",  ii]+bkgd_2["q_rep heifer", ii]

  # temporarily remove herd replacement from total available supply
  bkgd_2["q_sup steer",  ii] <- bkgd_2["q_sup steer",  ii] - bkgd_2["q_rep steer",  ii]
  bkgd_2["q_sup heifer", ii] <- bkgd_2["q_sup heifer", ii] - bkgd_2["q_rep heifer", ii]

  ## SHOCK: RESTRICTED DAIRY FEEDER MOVEMENT WITHIN PROVINCE
  ## ROUGH ESTIMATE: GREY AND BRUCE COUNTIES ACCOUNT FOR 25% OF DAIRY FEEDERS
  # add dairy feeders to total available supply
  bkgd_2["q_sup steer",  ii] <- bkgd_2["q_sup steer",  ii] + max(round(.75*assume2$dairy_fdr*fit(qq$bkgrd$dairy$data, logeq = F)$yhat)[i], 0)
  bkgd_2["q_sup heifer", ii] <- bkgd_2["q_sup heifer", ii] + max(round(.75*(1-assume2$dairy_fdr)*fit(qq$bkgrd$dairy$data, logeq = F)$yhat)[i], 0)
  bkgd_2["q_sup total",  ii] <- bkgd_2["q_sup steer",  ii] + bkgd_2["q_sup heifer", ii]

  ## SHOCK BORDER SHUT DOWN AFFECTS FEEDER EXPORTS TO U.S. AND INTERPROVINCIAL IMPORTS
  exc_dem <- sign(bkgd_2["q_dem total",  ii] - bkgd_2["q_sup total",  ii])

  # excess demand: demand by gender is now supply net replacements
  if (exc_dem == 1){

    bkgd_2["q_dem steer",  ii] <- bkgd_2["q_sup steer",  ii]
    bkgd_2["q_dem heifer", ii] <- bkgd_2["q_sup heifer", ii]
    bkgd_2["q_dem total",  ii] <- bkgd_2["q_sup total",  ii]

  }

  # excess supply: gender determined by ratio of heifers to steers in provincial finishing supply
  if (exc_dem == -1){

    # just for gender ratio
    fnsh_2["q_sup steer",  ii] <- round(fit(log_data(qq$fnshg$steer$supply$data))$yhat[i])
    fnsh_2["q_sup heifer", ii] <- round(fit(log_data(qq$fnshg$heifer$supply$data))$yhat[i])
    fnsh_2["q_sup total",  ii] <- fnsh_2["q_sup steer",  ii]+fnsh_2["q_sup heifer",  ii]

    # calculate background demand by gender
    bkgd_2["q_dem steer",  ii] <- round(bkgd_2["q_dem total",  ii]*(fnsh_2["q_sup steer",  ii]/fnsh_2["q_sup total",  ii]))
    bkgd_2["q_dem heifer", ii] <- round(bkgd_2["q_dem total",  ii]*(fnsh_2["q_sup heifer", ii]/fnsh_2["q_sup total",  ii]))

    # impose market clearing condition on excess supply
    bkgd_2["q_sup steer",  ii] <- bkgd_2["q_dem steer",  ii]
    bkgd_2["q_sup heifer", ii] <- bkgd_2["q_dem heifer", ii]
    bkgd_2["q_sup total",  ii] <- bkgd_2["q_dem total",  ii]

  }

  # feeder exports to U.S. by gender RESTRICTED
  bkgd_2["q_xus total",  ii] <- 0 #round(fit(qq$bkgrd$export$data, logeq = F)$yhat)[i]
  bkgd_2["q_xus steer",  ii] <- 0 #round(bkgd_2["q_xus total",  ii]*assume2$exportfdr)
  bkgd_2["q_xus heifer", ii] <- 0 #round(bkgd_2["q_xus total",  ii]*(1-assume2$exportfdr))

  # interprovincial imports RESTRICTED
  bkgd_2["q_mip steer",  ii] <- 0 #bkgd_2["q_dem steer",   ii] + bkgd_2["q_xus steer",   ii] - bkgd_2["q_sup steer",   ii] - round(dairy*assume2$dairy_fdr)
  bkgd_2["q_mip heifer", ii] <- 0 #bkgd_2["q_dem heifer",  ii] + bkgd_2["q_xus heifer",  ii] - bkgd_2["q_sup heifer",  ii] - round(dairy*(1-assume2$dairy_fdr))
  bkgd_2["q_mip total",  ii] <- 0 #bkgd_2["q_mip steer",   ii] + bkgd_2["q_mip heifer",  ii]

  # add replacements back to supply
  bkgd_2["q_sup steer",  ii] <- bkgd_2["q_sup steer",  ii] + bkgd_2["q_rep steer",  ii]
  bkgd_2["q_sup heifer", ii] <- bkgd_2["q_sup heifer", ii] + bkgd_2["q_rep heifer", ii]
  bkgd_2["q_sup total",  ii] <- bkgd_2["q_sup steer",  ii] + bkgd_2["q_sup heifer", ii]

  ## REMOVE TEMPORARY VARIABLES
  rm(tmp1, den1, num1, num2)





  ###############################
  #### FINISHING
  ###############################

  ## BASE QUANTITIES

  # first iteration: load supply and demand by gender
  if (it == 1){

    ## SHOCK: INCORPORATE STOMP OUT
    fnsh_2["q_sup steer",  ii] <- round((1-assume2$stomp$fnsh)*fit(log_data(qq$fnshg$steer$supply$data))$yhat[i])
    fnsh_2["q_sup heifer", ii] <- round((1-assume2$stomp$fnsh)*fit(log_data(qq$fnshg$heifer$supply$data))$yhat[i])
    fnsh_2["q_dem steer",  ii] <- round((1-assume2$stomp$fnsh)*fit(log_data(qq$fnshg$steer$demand$data))$yhat[i])
    fnsh_2["q_dem heifer", ii] <- round((1-assume2$stomp$fnsh)*fit(log_data(qq$fnshg$heifer$demand$data))$yhat[i])

  }

  if (it >= 2){

    # steer supply
    delta_q1   <- (fnsh_2["price steer", it-1] - fnsh_1["price steer", it-1])/fnsh_1["price steer", it-1]
    coef1      <- fit(log_data(qq$fnshg$steer$supply$data))$coef["xSteer.Price....100.lbs....t.1."]
    delta_q2   <- (bkgd_2["price steer", it-1] - bkgd_1["price steer", it-1])/bkgd_1["price steer", it-1]
    coef2      <- fit(log_data(qq$fnshg$steer$supply$data))$coef["xRail.Grade.Feeder.Steer.Price....100.lbs....t.1."]
    delta_q3   <- (fnsh_2["q_sup total", it-1]-fnsh_1["q_sup total", it-1])/fnsh_1["q_sup total", it-1]
    coef3      <- fit(log_data(qq$fnshg$steer$supply$data))$coef["xSteers.t.1."]
    net_effect <- (1+delta_q1*coef1)*(1+delta_q2*coef2)*(1+delta_q3*coef3)
    fnsh_2["q_sup steer",  ii] <- round(net_effect*fit(log_data(qq$fnshg$steer$supply$data))$yhat[i])

    # heifer supply
    if (it == 2)  fnsh_2["q_sup heifer", ii] <- round(fit(log_data(qq$fnshg$heifer$supply$data))$yhat[i])
    if (it > 2){

      delta_q1   <- (fnsh_2["price heifer", it-2] - fnsh_1["price heifer", it-2])/fnsh_1["price heifer", it-2]
      coef1      <- fit(log_data(qq$fnshg$heifer$supply$data))$coef["xHeifer.Price....100.lbs....t.2."]
      delta_q2   <- (bkgd_2["price heifer", it-2] - bkgd_1["price heifer", it-2])/bkgd_1["price heifer", it-2]
      coef2      <- fit(log_data(qq$fnshg$heifer$supply$data))$coef["xFeeder.Heifer.Rail.Grade.Price....100.lbs....t.2."]
      net_effect <- (1+delta_q1*coef1)*(1+delta_q2*coef2)
      fnsh_2["q_sup heifer", ii] <- round(net_effect*fit(log_data(qq$fnshg$heifer$supply$data))$yhat[i])

    }

    # steer demand (xFed.Steers..t.1.)
    # not sure how to handle xSteer.Price....100.lbs..  xAAA.Boxed.Beef.Price....lbs..
    delta_q1 <- (fnsh_2["q_sup steer", it-1] - fnsh_1["q_sup steer", it-1])/fnsh_1["q_sup steer", it-1]
    coef1    <- fit(log_data(qq$fnshg$steer$demand$data))$coef["xFed.Steers..t.1."]
    fnsh_2["q_dem steer",  ii] <- round((1+delta_q1*coef1)*fit(log_data(qq$fnshg$steer$demand$data))$yhat[i])

    # heifer demand (xFed.Heifers..t.1.)
    # not sure how to handle xHeifer.Price....100.lbs.. xAAA.Boxed.Beef.Price....lbs.
    delta_q1 <- (fnsh_2["q_sup heifer", it-1] - fnsh_1["q_sup heifer", it-1])/fnsh_1["q_sup heifer", it-1]
    coef1    <- fit(log_data(qq$fnshg$heifer$demand$data))$coef["xFed.Heifers..t.1."]
    fnsh_2["q_dem heifer", ii] <- round((1+delta_q1*coef1)*fit(log_data(qq$fnshg$heifer$demand$data))$yhat[i])

    rm(delta_q1, coef1, delta_q2, coef2, delta_q3, coef3)
  }



  ## DERIVED QUANTITIES

  # provincial supply and demand
  fnsh_2["q_sup total",  ii] <- fnsh_2["q_sup steer",  ii]+fnsh_2["q_sup heifer",  ii]
  fnsh_2["q_dem total",  ii] <- fnsh_2["q_dem steer",  ii]+fnsh_2["q_dem heifer",  ii]


  ## SHOCK BORDER SHUT DOWN AFFECTS FEEDER EXPORTS TO U.S. AND INTERPROVINCIAL IMPORTS
  exc_dem <- sign(fnsh_2["q_dem total", ii] - fnsh_2["q_sup total", ii])

  if (exc_dem ==  1){

    fnsh_2["q_dem total",  ii] <- fnsh_2["q_sup total",  ii]
    fnsh_2["q_dem heifer", ii] <- fnsh_2["q_sup heifer", ii]
    fnsh_2["q_dem steer",  ii] <- fnsh_2["q_sup steer",  ii]

  }

  if (exc_dem == -1){

    fnsh_2["q_sup total",  ii] <- fnsh_2["q_dem total",  ii]
    fnsh_2["q_sup heifer", ii] <- fnsh_2["q_dem heifer", ii]
    fnsh_2["q_sup steer",  ii] <- fnsh_2["q_dem steer",  ii]

  }

  # residual slaughter capacity (in head per quarter)
  spare_capacity2 <- max(assume2$capacity - fnsh_2["q_sup total", ii], 0)


  # load exports to u.s. by gender
  fnsh_2["q_xus steer",  ii] <- 0 #round(fit(qq$fnshg$steer$export$data, logeq = F)$yhat[i])
  fnsh_2["q_xus heifer", ii] <- 0 #round(fit(qq$fnshg$heifer$export$data, logeq = F)$yhat[i])
  fnsh_2["q_xus total",  ii] <- 0 #fnsh_2["q_xus steer",  ii]+fnsh_2["q_xus heifer",  ii]

  # calculate interprovincial imports (mip) as residual
  fnsh_2["q_mip steer",  ii] <- 0 #fnsh_2["q_dem steer",   ii] + fnsh_2["q_xus steer",   ii] - fnsh_2["q_sup steer",   ii]
  fnsh_2["q_mip heifer", ii] <- 0 #fnsh_2["q_dem heifer",  ii] + fnsh_2["q_xus heifer",  ii] - fnsh_2["q_sup heifer",  ii]
  fnsh_2["q_mip total",  ii] <- 0 #fnsh_2["q_mip steer",   ii] + fnsh_2["q_mip heifer",  ii]





  ###############################
  #### NON-FED MARKET
  ###############################

  ## BASE QUANTITIES

  if (it == 1) {


    # demand by gender
    cull_2["q_dem bull",  ii] <- round(fit(log_data(qq$culld$bulls$demand$data))$yhat[i])
    cull_2["q_dem cow",   ii] <- round(fit(log_data(qq$culld$cows$demand$data))$yhat[i])

    ## SHOCK: INCORPORATE STOMP OUT
    # load non-fed cow and bull inventory
    cull_2["q_inv bull",   ii] <- (1-assume2$stomp$cull)*inventory1["cull bull", ii]
    cull_2["q_inv cow",    ii] <- (1-assume2$stomp$cull)*inventory1["cull cow",  ii]

  }

  if (it >= 2){

    # inventory response bulls
    delta_q <- (cull_2["q_inv bull", it-1] - cull_1["q_inv bull", it-1])/cull_1["q_inv bull", it-1]
    coef    <- fit(log_data(qq$culld$bulls$inventory$data))$coef["xInventory.Bulls..t.1."]
    cull_2["q_inv bull",  ii] <- round((1+delta_q*coef)*fit(log_data(qq$culld$bulls$inventory$data))$yhat[i])

    # inventory response cows
    delta_q <- (cull_2["q_inv cow", it-1] - cull_1["q_inv cow", it-1])/cull_1["q_inv cow", it-1]
    coef    <- fit(log_data(qq$culld$cows$inventory$data))$coef["xInventory.Beef.Cows..t.1."]
    cull_2["q_inv cow",  ii] <- round((1+delta_q*coef)*fit(log_data(qq$culld$cows$inventory$data))$yhat[i])

    # demand bull (xSlaughter.Bulls..t.1.)
    delta_q <- (cull_2["q_dem bull", it-1] - cull_1["q_dem bull", it-1])/cull_1["q_dem bull", it-1]
    coef    <- fit(log_data(qq$culld$bulls$demand$data))$coef["xSlaughter.Bulls..t.1."]
    cull_2["q_dem bull",  ii] <- round((1+delta_q*coef)*fit(log_data(qq$culld$bulls$demand$data))$yhat[i])

    # demand cow (xSlaughter.Cows..t.1.)
    delta_q <- (cull_2["q_dem cow", it-1] - cull_1["q_dem cow", it-1])/cull_1["q_dem cow", it-1]
    coef    <- fit(log_data(qq$culld$cows$demand$data))$coef["xSlaughter.Cows..t.1."]
    cull_2["q_dem cow",  ii] <- round((1+delta_q*coef)*fit(log_data(qq$culld$cows$demand$data))$yhat[i])

    rm (delta_q, coef)
  }



  ## DERIVED QUANTITIES

  # total provincial inventory
  cull_2["q_inv total",  ii] <- cull_2["q_inv bull",  ii] + cull_2["q_inv cow",  ii]

  # provincial demand
  cull_2["q_dem total", ii] <- cull_2["q_dem bull",  ii]+cull_2["q_dem cow", ii]

  ## SHOCK -- BORDER CLOSURES
  cull_2["q_xus bull",  ii] <- 0 #round(fit(qq$culld$bulls$export$data, logeq = F)$yhat[i])
  cull_2["q_xus cow",   ii] <- 0 #round(fit(qq$culld$cows$export$data, logeq = F)$yhat[i])
  cull_2["q_xus total", ii] <- 0 #cull_2["q_xus bull",  ii]+cull_2["q_xus cow", ii]


  # apply slaughter capacity constraint to demand (seperately for each gender)
  temp <- cull_2["q_dem bull",  ii]/cull_2["q_dem total",  ii]

  if (cull_2["q_dem bull",  ii] > temp*spare_capacity2) {

    cull_2["q_dem bull",  ii] <- temp*spare_capacity2
    cull_2["capacity constraint", ii] <- 1

  }

  if (cull_2["q_dem cow",   ii] > (1-temp)*spare_capacity2) {

    cull_2["q_dem cow",   ii] <- (1-temp)*spare_capacity2
    cull_2["capacity constraint", ii] <- 1

  }

  cull_2["q_dem total", ii] <- cull_2["q_dem bull",  ii]+cull_2["q_dem cow", ii]

  # supply incorporates change in inventory
  # change in inventory is subject to some constraints
  # you can add as much inventory as you want
  # however, if the change in inventory is negative two constraints apply:
  # (1) you cannot draw more inventory than currently available
  # (2) you would not draw more inventory than the quantity demanded and exported
  # without these two constraints, you get weird negative values in the result

  # first calculate the change  (D = delta) in inventory
  if (it == 1) {

    temp   <- which(colnames(inventory1) == ii)
    Dinv_b <- cull_2["q_inv bull", it] - inventory1["cull bull", temp-1]
    Dinv_c <- cull_2["q_inv cow",  it] - inventory1["cull cow",  temp-1]

  }

  if (it >= 2) {

    Dinv_b <- cull_2["q_inv bull", it] - cull_2["q_inv bull", it-1]
    Dinv_c <- cull_2["q_inv cow",  it] - cull_2["q_inv cow",  it-1]

  }

  # if change in inventory is negative, apply restrictions
  if (Dinv_b < 0) {

    # (1) you cannot draw more inventory than currently available
    Dinv_b <- -1*min(abs(Dinv_b), cull_2["q_inv bull", it])

    # (2) you would not draw more inventory than the quantity demanded and exported
    if (abs(Dinv_b) > cull_2["q_dem bull", it] + cull_2["q_xus bull", it]) {

      Dinv_b <- -1*(cull_2["q_dem bull", it] + cull_2["q_xus bull", it])

      # add excess back to inventory
      if (it == 1) cull_2["q_inv bull", it] <- cull_2["q_inv bull", it] + abs(cull_2["q_inv bull", it] - inventory1["cull bull",  temp-1]) - abs(Dinv_c)
      if (it >= 2) cull_2["q_inv bull", it] <- cull_2["q_inv bull", it] + abs(cull_2["q_inv bull", it] - cull_2["q_inv bull", it-1]) - abs(Dinv_c)

    }

  }

  if (Dinv_c < 0) {

    # (1) you cannot draw more inventory than currently available
    Dinv_c <- -1*min(abs(Dinv_c), cull_2["q_inv cow", it])

    # (2) you would not draw more inventory than the quantity demanded and exported
    if (abs(Dinv_c) > cull_2["q_dem cow", it] + cull_2["q_xus cow", it]) {

      Dinv_c <- -1*(cull_2["q_dem cow", it] + cull_2["q_xus cow", it])

      # add excess back to inventory
      if (it == 1) cull_2["q_inv cow", it] <- cull_2["q_inv cow", it] + abs(cull_2["q_inv cow", it] - inventory1["cull cow",  temp-1]) - abs(Dinv_c)
      if (it >= 2) cull_2["q_inv cow", it] <- cull_2["q_inv cow", it] + abs(cull_2["q_inv cow", it] - cull_2["q_inv cow", it-1]) - abs(Dinv_c)

    }

  }

  # derive provincial supply
  cull_2["q_sup bull",  ii] <- cull_2["q_dem bull", ii] + cull_2["q_xus bull", ii] + Dinv_b
  cull_2["q_sup cow",   ii] <- cull_2["q_dem cow",  ii] + cull_2["q_xus cow",  ii] + Dinv_c
  cull_2["q_sup total", ii] <- cull_2["q_sup bull", ii] + cull_2["q_sup cow",  ii]


  # remove temporary inventory variables
  rm(temp, Dinv_b, Dinv_c)




  ###############################
  #### PROCESSED BEEF MARKET
  ###############################

  ## BASE QUANTITIES

  # first iteration: load provincial demand in lbs.
  if (it == 1) proc_2["q_dem total", ii] <- round(fit(log_data(qq$markt$demand$data))$yhat[i]*assume2$popn[i])

  if (it >= 2) {

    delta_q <- (proc_2["q_dem total", it-1] - proc_1["q_dem total", it-1])/proc_1["q_dem total", it-1]
    coef    <- fit(log_data(qq$markt$demand$data))$coef["xpcbeefdisappear..t.1."]
    proc_2["q_dem total", ii] <- round((1+delta_q*coef)*fit(log_data(qq$markt$demand$data))$yhat[i]*assume2$popn[i])
    rm(delta_q, coef)

  }

  ## DERIVED QUANTITIES

  # provincial supply in lbs.
  proc_2["q_sup total", ii] <- fnsh_2["q_sup heifer", ii]*weight[1, ii]
  proc_2["q_sup total", ii] <- fnsh_2["q_sup steer", ii]*weight[2, ii] + proc_2["q_sup total", ii]
  proc_2["q_sup total", ii] <- cull_2["q_sup cow", ii]*weight[3, ii] + proc_2["q_sup total", ii]
  proc_2["q_sup total", ii] <- round(cull_2["q_sup bull", ii]*weight[4, ii] + proc_2["q_sup total", ii])



  ## SHOCK -- NO PROCESSED BEEF MOVEMENT ACROSS BORDERS

  #   if (it < 3) {

  # exports
  proc_2["q_xus total", ii] <- 0 #round(fit(qq$markt$export$usa$data, logeq = F)$yhat[i])
  proc_2["q_xrw total", ii] <- 0 #round(fit(qq$markt$export$row$data, logeq = T)$yhat[i])

  # imports from U.S. and R.O.W. (excluding any shortage in provincial supply to be made up by the U.S.)
  proc_2["q_mus total", ii] <- round(fit(qq$markt$import$usa$data, logeq = F)$yhat[i])
  proc_2["q_mrw total", ii] <- round(fit(qq$markt$import$row$data, logeq = F)$yhat[i])

  ## market clearing condition
  # residual demand
  resid_dem <-  proc_2["q_dem total", ii] - proc_2["q_sup total", ii] + (proc_2["q_xus total", ii] + proc_2["q_xrw total", ii]) - (proc_2["q_mus total", ii] + proc_2["q_mrw total", ii])


  # slightly different than counterfactual-B
  # if residual demand is negative, reduce imports
  # if residual demand is positive, increase imports
  proc_2["q_mus total", ii] <- proc_2["q_mus total", ii] + resid_dem
  rm(resid_dem)

  #   }
  #
  #   if (it >= 3) {
  #
  #     # exports and imports
  #     proc_2["q_xus total", ii] <- round(fit(qq$markt$export$usa$data, logeq = F)$yhat[i])
  #     proc_2["q_xrw total", ii] <- round(fit(qq$markt$export$row$data, logeq = T)$yhat[i])
  #     proc_2["q_mus total", ii] <- round(fit(qq$markt$import$usa$data, logeq = F)$yhat[i])
  #     proc_2["q_mrw total", ii] <- round(fit(qq$markt$import$row$data, logeq = F)$yhat[i])
  #
  #     # interprovincial imports
  #     proc_2["q_mip total", ii] <- proc_2["q_dem total", ii] - proc_2["q_sup total", ii] + (proc_2["q_xus total", ii] + proc_2["q_xrw total", ii]) - (proc_2["q_mus total", ii] + proc_2["q_mrw total", ii])
  #
  #   }



  ###############################
  #### PRICE RESPONSES
  ###############################

  ## NOTE: Prices transmit from retail backwards through supply chain. Therefore start at processing sector.
  ## Also, price response divided into a price transmission effect (pt_eff) and change in quantity effects (qs_eff and qd_eff for supply and demand, respectively)

  ## PROCESSING SECTOR PRICE CHANGES
  # base prices
  proc_2["price retail", ii] <- round(fit(log_data(pp$markt$retail$data))$yhat[i], 2)
  proc_2["price wholes", ii] <- round(fit(log_data(pp$markt$wholesale$data))$yhat[i], 2)

  ## NOTE: There is nothing in the equations at the processed level that would change under this scenario
  # Therefore no effects at the processing price level


  ## FINISHING SECTOR PRICE CHANGES
  # base prices
  fnsh_2["price steer", ii] <- round(fit(log_data(pp$fnshg$steer$data))$yhat[i], 2)
  fnsh_2["price heifer", ii] <- round(fit(log_data(pp$fnshg$heifer$data))$yhat[i], 2)

  # lag price transmission effect
  c_pts <- 0
  c_pth <- 0
  d_pts <- 0
  d_pth <- 0

  if (it >= 2) {


    d_pts <- pct_chg(fnsh_1["price steer", it-1], fnsh_2["price steer", it-1])
    c_pts <- fit(log_data(pp$fnshg$steer$data))$coef["xSteer.Price....100.lbs....t.1."]

    d_pth <- pct_chg(fnsh_1["price heifer", it-1], fnsh_2["price heifer", it-1])
    c_pth <- fit(log_data(pp$fnshg$heifer$data))$coef["xHeifer.Price....100.lbs....t.1."]

  }

  # quantity demanded effects
  d_qds <- pct_chg(fnsh_1["q_dem steer",  ii], fnsh_2["q_dem steer",  ii])
  d_qdh <- pct_chg(fnsh_1["q_dem heifer", ii], fnsh_2["q_dem heifer", ii])

  # quantity supplied effects
  d_qss <- pct_chg(fnsh_1["q_sup steer",  ii], fnsh_2["q_sup steer",  ii])
  d_qsh <- pct_chg(fnsh_1["q_sup heifer", ii], fnsh_2["q_sup heifer", ii])

  # net price effect
  dP_s <- (1+d_pts*c_pts)*(1+d_qds*eta1$id$fnsh$s)*(1+d_qss*eta1$is$fnsh$s)
  dP_h <- (1+d_pth*c_pth)*(1+d_qdh*eta1$id$fnsh$h)*(1+d_qsh*eta1$is$fnsh$h)
  fnsh_2["price steer",  ii] <- round(dP_s*fnsh_2["price steer",  ii], 2)
  fnsh_2["price heifer", ii] <- round(dP_h*fnsh_2["price heifer", ii], 2)




  ## CULLED SECTOR PRICE CHANGES
  # note: there are no transmissions across
  # base prices
  cull_2["price bull", ii] <- round(fit(log_data(pp$culld$steer$data))$yhat[i], 2)
  cull_2["price cow",  ii] <- round(fit(log_data(pp$culld$heifer$data))$yhat[i], 2)

  # lag price transmission effect
  c_pts <- 0
  c_pth <- 0
  d_pts <- 0
  d_pth <- 0

  if (it >= 2) {


    d_pts <- pct_chg(cull_1["price bull", it-1], cull_2["price bull", it-1])
    c_pts <- fit(log_data(pp$culld$steer$data))$coef["xBull.Price..t.1."]

    d_pth <- pct_chg(cull_1["price cow", it-1], cull_2["price cow", it-1])
    c_pth <- fit(log_data(pp$culld$heifer$data))$coef["xCow.Price....100.lbs....t.1."]

  }

  # quantity demanded effects
  d_qds <- pct_chg(cull_1["q_dem bull",  ii], cull_2["q_dem bull",  ii])
  d_qdh <- pct_chg(cull_1["q_dem cow", ii], cull_2["q_dem cow", ii])

  # quantity supplied effects
  d_qss <- pct_chg(cull_1["q_sup bull",  ii], cull_2["q_sup bull",  ii])
  d_qsh <- pct_chg(cull_1["q_sup cow", ii], cull_2["q_sup cow", ii])

  # net price effect
  dP_s <- (1+d_pts*c_pts)*(1+d_qds*eta1$id$cull$b)*(1+d_qss*eta1$is$cull$b)
  dP_h <- (1+d_pth*c_pth)*(1+d_qdh*eta1$id$cull$c)*(1+d_qsh*eta1$is$cull$c)
  if (d_qsh == -1) dP_h <- 1
  cull_2["price bull",  ii] <- round(dP_s*cull_2["price bull",  ii], 2)
  cull_2["price cow", ii] <- round(dP_h*cull_2["price cow", ii], 2)







  ## BACKGROUNDING SECTOR PRICE CHANGES
  # base prices
  bkgd_2["price steer",  ii] <- round(fit(log_data(pp$bkgrd$steer$data))$yhat[i], 2)
  bkgd_2["price heifer", ii] <- round(fit(log_data(pp$bkgrd$heifer$data))$yhat[i], 2)

  # price transmission effect: effect of fed cattle price change on culled cattle prices
  d_pts <- pct_chg(round(fit(log_data(pp$fnshg$steer$data))$yhat[i], 2), fnsh_2["price steer",  ii])
  c_pts <- fit(log_data(pp$bkgrd$steer$data))$coef["xSteer.Price....100.lbs.."]
  d_pth <- pct_chg(round(fit(log_data(pp$fnshg$heifer$data))$yhat[i], 2), fnsh_2["price heifer",  ii])
  c_pth <- fit(log_data(pp$bkgrd$heifer$data))$coef["xHeifer.Price....100.lbs.."]

  # quantity demanded effects
  d_qds <- pct_chg(bkgd_1["q_dem steer",  ii], bkgd_2["q_dem steer",  ii])
  d_qdh <- pct_chg(bkgd_1["q_dem heifer", ii], bkgd_2["q_dem heifer", ii])

  # quantity supplied effects
  d_qss <- pct_chg(bkgd_1["q_sup steer",  ii], bkgd_2["q_sup steer",  ii])
  d_qsh <- pct_chg(bkgd_1["q_sup heifer", ii], bkgd_2["q_sup heifer", ii])

  # net price effect
  dP_s <- (1+d_pts*c_pts)*(1+d_qds*eta1$id$bkgd$s)*(1+d_qss*eta1$is$bkgd$s)
  dP_h <- (1+d_pth*c_pth)*(1+d_qdh*eta1$id$bkgd$h)*(1+d_qsh*eta1$is$bkgd$h)
  bkgd_2["price steer",  ii] <- round(dP_s*bkgd_2["price steer",  ii], 2)
  bkgd_2["price heifer", ii] <- round(dP_h*bkgd_2["price heifer", ii], 2)




  ## CALF SECTOR PRICE CHANGES
  # base prices
  calf_2["price steer",  ii] <- round(fit(log_data(pp$calf$steer$data))$yhat[i], 2)
  calf_2["price heifer", ii] <- round(fit(log_data(pp$calf$heifer$data))$yhat[i], 2)

  # price transmission effect: effect of backgrounded cattle price change on culled cattle prices
  d_pts <- pct_chg(round(fit(log_data(pp$bkgrd$steer$data))$yhat[i], 2), bkgd_2["price steer",  ii])
  c_pts <- fit(log_data(pp$calf$steer$data))$coef["xFeeder.Steer.Price....100.lbs.."]
  d_pth <- pct_chg(round(fit(log_data(pp$bkgrd$heifer$data))$yhat[i], 2), bkgd_2["price heifer",  ii])
  c_pth <- fit(log_data(pp$calf$heifer$data))$coef["xFeeder.Heifer.Railgrade.Price....100.lbs.."]

  # quantity demanded effects
  d_qds <- pct_chg(calf_1["q_dem steer",  ii], calf_2["q_dem steer",  ii])
  d_qdh <- pct_chg(calf_1["q_dem heifer", ii], calf_2["q_dem heifer", ii])

  # quantity supplied effects
  d_qss <- pct_chg(calf_1["q_sup steer",  ii], calf_2["q_sup steer",  ii])
  d_qsh <- pct_chg(calf_1["q_sup heifer", ii], calf_2["q_sup heifer", ii])

  # net price effect
  dP_s <- (1+d_pts*c_pts)*(1+d_qds*eta1$id$calf$s)*(1+d_qss*eta1$is$calf$s)
  dP_h <- (1+d_pth*c_pth)*(1+d_qdh*eta1$id$calf$h)*(1+d_qsh*eta1$is$calf$h)
  calf_2["price steer",  ii] <- round(dP_s*calf_2["price steer",  ii], 2)
  calf_2["price heifer", ii] <- round(dP_h*calf_2["price heifer", ii], 2)


  rm(c_pts, c_pth, d_pth, d_pts, d_qdh, d_qds, d_qsh, d_qss, dP_h, dP_s)

}


# calf_2
# bkgd_2
# fnsh_2
# cull_2
# proc_2
