###############################
#### CHANGES IN WELFARE 
###############################

# load functions
source("beef/code-version2/functions.R")

# run 
source("beef/code-version2/step-2 counterfactual-A (federal zoning).R")

# matrix to store welfare changes
welfare <- list()

for (it in 1:ncol(calf_1)){
  
  welfare[[it]] <- matrix(0, 5, 5, dimnames=list(c("calf", "bkgd", "fnsh", "cull", "proc"), c("base CS", "counter CS", "base PS", "counter PS", "delta net")))

  
  ## cow-calf
  # calculations (using function)
  tmp1 <- welfare_calc(calf_1["price steer",  it], calf_1["q_dem steer",  it], calf_1["q_sup steer",  it], eta1$d$calf, eta1$s$calf)
  tmp2 <- welfare_calc(calf_2["price steer",  it], calf_2["q_dem steer",  it], calf_2["q_sup steer",  it], eta1$d$calf, eta1$s$calf)
  tmp3 <- welfare_calc(calf_1["price heifer", it], calf_1["q_dem heifer", it], calf_1["q_sup heifer", it], eta1$d$calf, eta1$s$calf)
  tmp4 <- welfare_calc(calf_2["price heifer", it], calf_2["q_dem heifer", it], calf_2["q_sup heifer", it], eta1$d$calf, eta1$s$calf)
  cs_1 <- tmp1$cs + tmp3$cs
  cs_2 <- tmp2$cs + tmp4$cs
  ps_1 <- tmp1$ps + tmp3$ps
  ps_2 <- tmp2$ps + tmp4$ps
  
  # change in welfare
  welfare[[it]]["calf", "base CS"]    <- cs_1
  welfare[[it]]["calf", "counter CS"] <- cs_2
  welfare[[it]]["calf", "base PS"]    <- ps_1
  welfare[[it]]["calf", "counter PS"] <- ps_2
  welfare[[it]]["calf", "delta net"]  <- (cs_2-cs_1)+(ps_2-ps_1)
  
  
  
  ## backgrounding
  # calculations (using function)
  tmp1 <- welfare_calc(bkgd_1["price steer",  it], bkgd_1["q_dem steer",  it], bkgd_1["q_sup steer",  it], eta1$d$bkgd, eta1$s$bkgd)
  tmp2 <- welfare_calc(bkgd_2["price steer",  it], bkgd_2["q_dem steer",  it], bkgd_2["q_sup steer",  it], eta1$d$bkgd, eta1$s$bkgd)
  tmp3 <- welfare_calc(bkgd_1["price heifer", it], bkgd_1["q_dem heifer", it], bkgd_1["q_sup heifer", it], eta1$d$bkgd, eta1$s$bkgd)
  tmp4 <- welfare_calc(bkgd_2["price heifer", it], bkgd_2["q_dem heifer", it], bkgd_2["q_sup heifer", it], eta1$d$bkgd, eta1$s$bkgd)
  cs_1 <- tmp1$cs + tmp3$cs
  cs_2 <- tmp2$cs + tmp4$cs
  ps_1 <- tmp1$ps + tmp3$ps
  ps_2 <- tmp2$ps + tmp4$ps
  
  # change in welfare
  welfare[[it]]["bkgd", "base CS"]    <- cs_1
  welfare[[it]]["bkgd", "counter CS"] <- cs_2
  welfare[[it]]["bkgd", "base PS"]    <- ps_1
  welfare[[it]]["bkgd", "counter PS"] <- ps_2
  welfare[[it]]["bkgd", "delta net"]  <- (cs_2-cs_1)+(ps_2-ps_1)
  
  
  
  ## finishing
  # calculations (using function)
  tmp1 <- welfare_calc(fnsh_1["price steer",  it], fnsh_1["q_dem steer",  it], fnsh_1["q_sup steer",  it], eta1$d$fnsh$s, eta1$s$fnsh$s)
  tmp2 <- welfare_calc(fnsh_2["price steer",  it], fnsh_2["q_dem steer",  it], fnsh_2["q_sup steer",  it], eta1$d$fnsh$s, eta1$s$fnsh$s)
  tmp3 <- welfare_calc(fnsh_1["price heifer", it], fnsh_1["q_dem heifer", it], fnsh_1["q_sup heifer", it], eta1$d$fnsh$h, eta1$s$fnsh$h)
  tmp4 <- welfare_calc(fnsh_2["price heifer", it], fnsh_2["q_dem heifer", it], fnsh_2["q_sup heifer", it], eta1$d$fnsh$h, eta1$s$fnsh$h)
  cs_1 <- tmp1$cs + tmp3$cs
  cs_2 <- tmp2$cs + tmp4$cs
  ps_1 <- tmp1$ps + tmp3$ps
  ps_2 <- tmp2$ps + tmp4$ps
  
  # change in welfare
  welfare[[it]]["fnsh", "base CS"]    <- cs_1
  welfare[[it]]["fnsh", "counter CS"] <- cs_2
  welfare[[it]]["fnsh", "base PS"]    <- ps_1
  welfare[[it]]["fnsh", "counter PS"] <- ps_2
  welfare[[it]]["fnsh", "delta net"]  <- (cs_2-cs_1)+(ps_2-ps_1)
  
  
  
  ## culled market
  # calculations (using function)
  tmp1 <- welfare_calc(cull_1["price bull",  it], cull_1["q_dem bull",  it], cull_1["q_sup bull",  it], eta1$d$cull$b, eta1$s$cull$b)
  tmp2 <- welfare_calc(cull_2["price bull",  it], cull_2["q_dem bull",  it], cull_2["q_sup bull",  it], eta1$d$cull$b, eta1$s$cull$b)
  tmp3 <- welfare_calc(cull_1["price cow", it], cull_1["q_dem cow", it], cull_1["q_sup cow", it], eta1$d$cull$c, eta1$s$cull$c)
  tmp4 <- welfare_calc(cull_2["price cow", it], cull_2["q_dem cow", it], cull_2["q_sup cow", it], eta1$d$cull$c, eta1$s$cull$c)
  cs_1 <- tmp1$cs + tmp3$cs
  cs_2 <- tmp2$cs + tmp4$cs
  ps_1 <- tmp1$ps + tmp3$ps
  ps_2 <- tmp2$ps + tmp4$ps
  
  # change in welfare
  welfare[[it]]["cull", "base CS"]    <- cs_1
  welfare[[it]]["cull", "counter CS"] <- cs_2
  welfare[[it]]["cull", "base PS"]    <- ps_1
  welfare[[it]]["cull", "counter PS"] <- ps_2
  welfare[[it]]["cull", "delta net"]  <- (cs_2-cs_1)+(ps_2-ps_1)
  
 
  ## processed market
  # calculations (using function)
  tmp1 <- proc_1["q_dem total", it]
  tmp2 <- proc_1["q_sup total", it] + (proc_1["q_xus total", ii] + proc_1["q_xrw total", ii]) - (proc_1["q_mus total", ii] + proc_1["q_mrw total", ii]) - proc_1["q_mip total", ii]
  tmp3 <- welfare_calc(proc_1["price retail",  it], tmp1, tmp2, eta1$d$retail, abs(eta1$s$retail))
  cs_1 <- tmp3$cs
  ps_1 <- tmp3$ps
  
  tmp1 <- proc_2["q_dem total", it] 
  tmp2 <- proc_2["q_sup total", it] + (proc_2["q_xus total", ii] + proc_2["q_xrw total", ii]) - (proc_2["q_mus total", ii] + proc_2["q_mrw total", ii]) - proc_2["q_mip total", ii]
  tmp3 <- welfare_calc(proc_2["price retail",  it], tmp1, tmp2, eta1$d$retail, abs(eta1$s$retail))
  cs_2 <- tmp3$cs
  ps_2 <- tmp3$ps
  rm(tmp1, tmp2, tmp3)
  
  # change in welfare
  welfare[[it]]["proc", "base CS"]    <- cs_1
  welfare[[it]]["proc", "counter CS"] <- cs_2
  welfare[[it]]["proc", "base PS"]    <- ps_1
  welfare[[it]]["proc", "counter PS"] <- ps_2
  welfare[[it]]["proc", "delta net"]  <- (cs_2-cs_1)+(ps_2-ps_1)
  
  ## round 
  welfare[[it]] <- round(welfare[[it]])
}

## annual welfare
welfare_yr     <- welfare[[1]]
names(welfare) <- colnames(calf_1)
welfare_yr[]   <- 0
for (i in 1:nrow(welfare_yr)) welfare_yr[i, ] <- welfare[[1]][i, ] + welfare[[2]][i, ] + welfare[[3]][i, ] + welfare[[4]][i, ]
round(welfare_yr/1e6, 2)
colSums(round(welfare_yr/1e6, 2))
