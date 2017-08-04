scenario <- "B"

# function to create a summary table
summary <- function(base, counterfactual) { cbind(base, counterfactual, (counterfactual-base)/base)}

# function to change quarterly results to annual
annual <- function(x) { c(round(rowMeans(x)[1:2], 2), rowSums(x)[3:nrow(x)])}

# cow-calf
calf_sum <- summary(annual(calf_1), annual(calf_2))

# backgrounding
bkgd_sum <- summary(annual(bkgd_1), annual(bkgd_2))

# finsihing (fed) 
fnsh_sum <- summary(annual(fnsh_1), annual(fnsh_2))

# culled cattle
cull_sum <- summary(annual(cull_1), annual(cull_2))

# processed beef
proc_sum <- summary(annual(proc_1), annual(proc_2))


# write.csv(calf_sum, paste("beef/results/calf", "-", scenario, ".csv", sep=""))
# write.csv(bkgd_sum, paste("beef/results/bkgd", "-", scenario, ".csv", sep=""))
# write.csv(fnsh_sum, paste("beef/results/fnsh", "-", scenario, ".csv", sep=""))
# write.csv(cull_sum, paste("beef/results/cull", "-", scenario, ".csv", sep=""))
# write.csv(proc_sum, paste("beef/results/proc", "-", scenario, ".csv", sep=""))


#### summary tables in latex format ----

require(xtable)

## market impacts

# cow-calf
calf_sum[, 3] <- paste("(", round(100*calf_sum[, 3], 2), "%)", sep="")
xtable(calf_sum[c(1, 2, 3, 6, 9), ])

# backgrounding 
bkgd_sum[, 3] <- paste("(", round(100*bkgd_sum[, 3], 2), "%)", sep="")
xtable(bkgd_sum[c(1, 2, 3, 6, 9, 12, 15), ])

# finishing 
fnsh_sum[, 3] <- paste("(", round(100*fnsh_sum[, 3], 2), "%)", sep="")
xtable(fnsh_sum[c(1, 2, 3, 6, 9, 12), ])

# culled (non-fed)
cull_sum[, 3] <- paste("(", round(100*cull_sum[, 3], 2), "%)", sep="")
xtable(cull_sum[c(1, 2, 3, 6, 9, 12), ])

# processing 
proc_sum[3:nrow(proc_sum), 1:2] <- round(as.numeric(proc_sum[3:nrow(proc_sum), 1:2])/1e6, 1)
proc_sum[, 3] <- paste("(", round(100*proc_sum[, 3], 2), "%)", sep="")
xtable(proc_sum)

## welfare impacts ---- 

welf_sum1 <- round(welfare_yr/1e6, 2)


welf_sum2 <- rbind(
  cbind(welf_sum1[1, 1], welf_sum1[1, 2], 100*(welf_sum1[1, 2]-welf_sum1[1, 1])/welf_sum1[1, 1]),
  cbind(welf_sum1[1, 3], welf_sum1[1, 4], 100*(welf_sum1[1, 4]-welf_sum1[1, 3])/welf_sum1[1, 3]),
  rep(0, 3),
  cbind(welf_sum1[2, 1], welf_sum1[2, 2], 100*(welf_sum1[2, 2]-welf_sum1[2, 1])/welf_sum1[2, 1]),
  cbind(welf_sum1[2, 3], welf_sum1[2, 4], 100*(welf_sum1[2, 4]-welf_sum1[2, 3])/welf_sum1[2, 3]),
  rep(0, 3),
  cbind(welf_sum1[3, 1], welf_sum1[3, 2], 100*(welf_sum1[3, 2]-welf_sum1[3, 1])/welf_sum1[3, 1]),
  cbind(welf_sum1[3, 3], welf_sum1[3, 4], 100*(welf_sum1[3, 4]-welf_sum1[3, 3])/welf_sum1[3, 3]),
  rep(0, 3),
  cbind(welf_sum1[4, 1], welf_sum1[4, 2], 100*(welf_sum1[4, 2]-welf_sum1[4, 1])/welf_sum1[4, 1]),
  cbind(welf_sum1[4, 3], welf_sum1[4, 4], 100*(welf_sum1[4, 4]-welf_sum1[4, 3])/welf_sum1[4, 3]),
  rep(0, 3),
  cbind(welf_sum1[5, 1], welf_sum1[5, 2], 100*(welf_sum1[5, 2]-welf_sum1[5, 1])/welf_sum1[5, 1]),
  cbind(welf_sum1[5, 3], welf_sum1[5, 4], 100*(welf_sum1[5, 4]-welf_sum1[5, 3])/welf_sum1[5, 3]),
  rep(0, 3)
)

# cow-calf
welf_sum2[3,  ] <- cbind(welf_sum2[1, 1]+welf_sum2[2, 1], welf_sum2[1, 2]+welf_sum2[2, 2], 0)
welf_sum2[3, 3] <- 100*(welf_sum2[3, 2] - welf_sum2[3, 1])/welf_sum2[3, 1] 

# backgrounding
welf_sum2[6,  ] <- cbind(welf_sum2[4, 1]+welf_sum2[5, 1], welf_sum2[4, 2]+welf_sum2[5, 2], 0)
welf_sum2[6, 3] <- 100*(welf_sum2[6, 2] - welf_sum2[6, 1])/welf_sum2[6, 1] 

# finishing
welf_sum2[9,  ] <- cbind(welf_sum2[7, 1]+welf_sum2[8, 1], welf_sum2[7, 2]+welf_sum2[8, 2], 0)
welf_sum2[9, 3] <- 100*(welf_sum2[9, 2] - welf_sum2[9, 1])/welf_sum2[9, 1] 

# non-fed
welf_sum2[12,  ] <- cbind(welf_sum2[10, 1]+welf_sum2[11, 1], welf_sum2[10, 2]+welf_sum2[11, 2], 0)
welf_sum2[12, 3] <- 100*(welf_sum2[12, 2] - welf_sum2[12, 1])/welf_sum2[12, 1] 

# processing
welf_sum2[15,  ] <- cbind(welf_sum2[13, 1]+welf_sum2[14, 1], welf_sum2[13, 2]+welf_sum2[14, 2], 0)
welf_sum2[15, 3] <- 100*(welf_sum2[15, 2] - welf_sum2[15, 1])/welf_sum2[15, 1] 

welf_sum2[, 3] <- paste("(", round(welf_sum2[, 3], 2), "%)", sep="")

xtable(welf_sum2)


welf_sum3 <- welf_sum2[c(3, 6, 9, 12, 15), ]
welf_sum3 <- rbind(welf_sum3, c(sum(as.numeric(welf_sum3[, 1])), sum(as.numeric(welf_sum3[, 2])), 0))
welf_sum3[6, 3] <- paste("(", round(100*(as.numeric(welf_sum3[6, 2])-as.numeric(welf_sum3[6, 1]))/as.numeric(welf_sum3[6, 1]), 2), "%)", sep="")
welf_sum3[6, ]
