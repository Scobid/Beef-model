log_data <- function(data){

  for (j in 1:ncol(data)) {

    if(sum(data[, j]==0)<1) data[, j] <- log(data[, j])

  }

  ## remove redundant intercept from data
  data <- data[, -which(colSums(data)==0)]

  return(data)

}




fit <- function(data, logeq=T){

  ## ols
  y <- data[, 1]
  x <- data[, 2:ncol(data)]
  f <- lm(y ~ x)

  ## outputs
  coef <- f$coefficients
  yhat <- rev(f$fitted.values[1:4])
  names(yhat) <- paste("Q", 1:4, sep="")

  if (logeq == T) yhat <- exp(yhat)

  ## values to return
  return(list(coef=coef, yhat=yhat))

}



fit_inv_eta <- function(y, x, add.col=F) {

  # make sure the length of y and x match
  if (length(y) > nrow(x)) y <- y[1:nrow(x)]
  if (length(y) < nrow(x)) x <- x[1:length(y), ]

  # add column to X matrix
  if (is.logical(add.col) == FALSE) {

    x1 <- as.matrix(add.col)
    if (is.numeric(add.col) == TRUE) x1 <- x1[1:nrow(x)]
    if (is.matrix(add.col) == TRUE)  x1 <- x1[1:nrow(x), ]
    x  <- cbind(x, x1)

  }

  # basic fit
  fit <- lm(y ~ x)

  # return coefficient
  eta <- fit$coefficients[2]
  names(eta) <- NULL
  return(eta)

}



welfare_calc <- function(price, qdem, qsup,  eta_d, eta_s) {

  # consumer surplus 0
  int <- price*(eta_d -1)/eta_d
  cs  <- .5*(int - price)*qdem

  # producer surplus 0
  int <- price*(eta_s - 1)/eta_s
  ps  <- .5*(price - int)*qsup
  if (int < 0) ps <- ps + .5*(int)*(-1*int*eta_s*qsup/price)

  return(list(cs=cs, ps=ps))

}



pct_chg <- function(beg, end){

  return((end-beg)/beg)

}
