
# ------ LOAD FUNCTIONS  ----- #

source("beef/code-version2/functions.R")



# ------ RESULT MATRICES  ----- #

## use qtr specified in base model to
col_nm <- paste("Q",1:4, sep="")


# cow-calf sector
row_nm <- c("total", "heifer", "steer")
row_nm <- c(paste("price", row_nm[2:3]), paste("q_sup", row_nm), paste("q_dem", row_nm), paste("q_mip", row_nm))
calf_1 <- matrix(0, length(row_nm), length(col_nm), dimnames=list(row_nm, col_nm))
calf_2 <- calf_1

# backgrounding sector
row_nm <- c("total", "heifer", "steer")
row_nm <- c(paste("price", row_nm[2:3]), paste("q_sup", row_nm), paste("q_rep", row_nm), paste("q_dem", row_nm), paste("q_mip", row_nm), paste("q_xus", row_nm))
bkgd_1 <- matrix(0, length(row_nm), length(col_nm), dimnames=list(row_nm, col_nm))
bkgd_2 <- bkgd_1

# finishing sector
row_nm <- c("total", "heifer", "steer")
row_nm <- c(paste("price", row_nm[2:3]), paste("q_sup", row_nm), paste("q_dem", row_nm), paste("q_mip", row_nm), paste("q_xus", row_nm))
fnsh_1 <- matrix(0, length(row_nm), length(col_nm), dimnames=list(row_nm, col_nm))
fnsh_2 <- fnsh_1

# culled sector
row_nm <- c("total", "cow", "bull")
row_nm <- c(paste("price", row_nm[2:3]), paste("q_sup", row_nm), paste("q_inv", row_nm), paste("q_dem", row_nm), paste("q_xus", row_nm), "capacity constraint")
cull_1 <- matrix(0, length(row_nm), length(col_nm), dimnames=list(row_nm, col_nm))
cull_2 <- cull_1

# end product market
row_nm <- c("price retail", "price wholes", "q_sup total", "q_dem total", "q_mip total", "q_mus total", "q_mrw total", "q_xus total", "q_xrw total")
proc_1 <- matrix(0, length(row_nm), length(col_nm), dimnames=list(row_nm, col_nm))
proc_2 <- proc_1

# remove temporary variables
rm(row_nm, col_nm)


# ------ ASSUMED PARAMETERS  ----- #

assume1 <- list()

## ratio assumptions (stated in terms of % steers unless otherwise noted)
assume1$birth     <- 0.51   # (cow-calf) share of male calves at birth two quarter agos
assume1$mip$calf  <- 0.30   # (cow-calf) share of male calves imported interprovincially
assume1$replace$h <- 0.10   # (backgrounding) share of heifers retained for replacement
assume1$replace$s <- 0.07   # (backgrounding) share of steers retained for replacement (i.e. replacement bulls)
assume1$dairy_fdr <- 1.00   # (backgrounding) share of feeders from dairy industry which are male
assume1$exportfdr <- 0.95   # (backgrounding) share of feeders exported to the US which are male     ########################### MISSING

## other assumptions
# slaughter capacity (units = head of cattle)
assume1$capacity  <- 160140

# provincial population
assume1$popn      <- c(13438807, 13464470,  13505900,  13546112)


# ------ CARCASS WEIGHTS ----- #


# load data and OLS fit (using custom function)
ww  <- readRDS("beef/data/clean/weights.RDS")
w_h <- fit(log_data(ww$heifer$data))
w_s <- fit(log_data(ww$steer$data))
w_c <- fit(log_data(ww$cow$data))
w_b <- fit(log_data(ww$bull$data))

# # weight elasticities
# eta_w      <- data.frame(endsale="cow-calf", item="heifer", description="own-weight", value=0, stringsAsFactors=F)
# eta_w[1, ] <- c("finishing", "heifer", "own-weight", round(w_h$coef[8], 5))
# eta_w[2, ] <- c("finishing", "heifer", "corn price", round(w_h$coef[5], 5))
# eta_w[3, ] <- c("finishing", "steer", "own-weight", round(w_s$coef[7], 5))
# eta_w[4, ] <- c("finishing", "steer", "corn price", round(w_s$coef[5], 5))
# eta_w[5, ] <- c("culled", "cow", "own-weight", round(w_c$coef[11], 5))
# eta_w[6, ] <- c("culled", "cow", "corn price ratio", round(w_c$coef[5], 5))
# eta_w[7, ] <- c("culled", "bull", "own-weight", round(w_b$coef[8], 5))
# eta_w[8, ] <- c("culled", "bull", "corn price ratio", round(w_b$coef[5], 5))

# weight levels
rw_nm       <- c(paste("finishing", c("heifer", "steer")), paste("culled", c("cow", "bull")))
weight      <- matrix(0, length(rw_nm), 4, dimnames=list(rw_nm, paste("Q", 1:4, sep="")))
weight[1, ] <- w_h$yhat
weight[2, ] <- w_s$yhat
weight[3, ] <- w_c$yhat
weight[4, ] <- w_b$yhat
weight      <- round(weight, 1)


# remove redundant variables
rm(ww, w_h, w_s, w_c, w_b, rw_nm)



# ------ REGRESSION ESTIMATES ----- #

pp  <- readRDS("beef/data/clean/price_transmissions.RDS")
qq   <- readRDS("beef/data/clean/quantities.RDS")





# ------ INVENTORIES ----- #


# matrix for storing inventories
inventory1 <- matrix(0, 4, 8, dimnames=list(c("calf supply", "calf mip", "cull bull", "cull cow"), c(paste("(Yr-1) Q", 1:4, sep=""), paste("Q", 1:4, sep=""))))

# calf supply
tmp1   <- log_data(qq$calf$supply$data)
inventory1["calf supply", ] <- round(exp(rev(lm(tmp1[, 1] ~ tmp1[, 3:ncol(tmp1)])$fitted.values[1:8])))

# calf mip
tmp2   <- log_data(qq$calf$demand$data)
inventory1["calf mip", ] <- (round(exp(rev(lm(tmp2[, 1] ~ tmp2[, 3:ncol(tmp2)])$fitted.values[1:8]))) - inventory1["calf supply", ])
rm(tmp1, tmp2)


# bull inventory
temp   <- log_data(qq$culld$bulls$inventory$data)
inventory1["cull bull", ] <- round(exp(rev(lm(temp[, 1] ~ temp[, 3:ncol(temp)])$fitted.values[1:8])))

# cow inventory
temp   <- log_data(qq$culld$cows$inventory$data)
inventory1["cull cow", ] <- round(exp(rev(lm(temp[, 1] ~ temp[, 3:ncol(temp)])$fitted.values[1:8])))
rm(temp)




# ------ INVERSE ELASTICITIES ----- #


## regular supply and demand price elasticities
eta1   <- list()
eta1$d <- list(calf=-1.650, bkgd=-0.196, fnsh=list(h=-0.470, s=-0.181), cull=list(c=-0.518, b=-0.073), retail=-0.713)
eta1$s <- list(calf= 0.359, bkgd= 0.623, fnsh=list(h= 0.069, s= 0.034), cull=list(c= 0.710, b= 0.710), retail=-0.045)


## calf-cow sector supply
x <- log_data(qq$calf$supply$data)
x <- x[, -ncol(x)]

# heifer
y <- log_data(pp$calf$heifer$data)[, 1]
eta1$is$calf$h <- fit_inv_eta(y, x)

# steer
y <- log_data(pp$calf$steer$data)[, 1]
eta1$is$calf$s <- fit_inv_eta(y, x)

## calf-cow sector demand
x <- log_data(qq$calf$demand$data)
x <- x[, -ncol(x)]

# heifer
y <- log_data(pp$calf$heifer$data)[, 1]
eta1$id$calf$h <- fit_inv_eta(y, x)

# steer
y <- log_data(pp$calf$steer$data)[, 1]
eta1$id$calf$s <- fit_inv_eta(y, x)



## backgrounding sector supply
x <- log_data(qq$bkgrd$supply$data)
x <- x[, -ncol(x)]
x <- x[, 1:4] # remove irrelevant dummy variables

# heifer
y <- log_data(pp$bkgrd$heifer$data)[, 1]
eta1$is$bkgd$h <- fit_inv_eta(y, x)

# steer
y <- log_data(pp$bkgrd$steer$data)[, 1]
eta1$is$bkgd$s <- fit_inv_eta(y, x)



## backgrounding sector demand
x <- log_data(qq$bkgrd$demand$data)
x <- x[, -ncol(x)]

# heifer
y <- log_data(pp$bkgrd$heifer$data)[, 1]
eta1$id$bkgd$h <- fit_inv_eta(y, x)

# steer
y <- log_data(pp$bkgrd$steer$data)[, 1]
eta1$id$bkgd$s <- fit_inv_eta(y, x)



## finishing sector supply
# heifer
x <- log_data(qq$fnshg$heifer$supply$data)
x <- x[, -ncol(x)]
x <- x[, 1:7] # remove irrelevant dummy variables
y <- log_data(pp$fnshg$heifer$data)[, 1]
eta1$is$fnsh$h <- fit_inv_eta(y, x, add.col = log_data(pp$fnshg$heifer$data)[, 7])

# steer
x <- log_data(qq$fnshg$steer$supply$data)
x <- x[, -ncol(x)]
x <- x[, 1:7] # remove irrelevant dummy variables
y <- log_data(pp$fnshg$steer$data)[, 1]
eta1$is$fnsh$s <- fit_inv_eta(y, x, add.col = log_data(pp$fnshg$steer$data)[, 7])




## finishing sector demand
# heifer
x <- log_data(qq$fnshg$heifer$demand$data)
x <- x[, -ncol(x)]
y <- log_data(pp$fnshg$heifer$data)[, 1]
eta1$id$fnsh$h <- fit_inv_eta(y, x)

# steer
x <- log_data(qq$fnshg$steer$demand$data)
x <- x[, -ncol(x)]
y <- log_data(pp$fnshg$steer$data)[, 1]
eta1$id$fnsh$s <- fit_inv_eta(y, x)



## culled sector inventory
eta1$is$cull$c <- (eta1$s$cull$c)^-1
eta1$is$cull$b <- (eta1$s$cull$b)^-1

## challenging estimation
# # cows
# x <- log_data(qq$culld$cows$inventory$data)
# y <- log_data(pp$culld$heifer$data)[, 1]
# x <- cbind(x, log_data(qq$fnshg$heifer$supply$data)[, "Ont..Corn.Price....Bushel...t.2."])
# x <- cbind(x, log_data(pp$fnshg$heifer$data)[1:nrow(x), 1])
# eta1$is$cull$c <- fit_inv_eta(y, x, log_data(pp$culld$heifer$data)[, 6])
#
# # bulls
# x <- log_data(qq$culld$bulls$inventory$data)[1:51, ]
# y <- log_data(pp$culld$steer$data)[, 1]
# x <- cbind(x, log_data(qq$fnshg$steer$supply$data)[, "Ont..Corn.Price....Bushel...t.2."])
# x <- cbind(x, log_data(pp$fnshg$steer$data)[1:nrow(x), 1])
# eta1$is$cull$b <- fit_inv_eta(y, x, log_data(pp$culld$steer$data)[, 6])


## culled sector demand
# again, challenging estimation (assume they are equal because bull is crazy big)
eta1$id$cull$c <- (eta1$d$cull$c)^-1
eta1$id$cull$b <- (eta1$d$cull$c)^-1
