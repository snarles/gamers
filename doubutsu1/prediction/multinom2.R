####
##  Multinomial prediction with interactions
####



if (!"allmoves" %in% ls()) source("doubutsu1/lg_analysis_setup.R")
# Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")
# Rcpp::sourceCpp("doubutsu1/prediction/interaction3.cpp")  ## use for order-3!!!





mcm_probs <- function(mat, bt) {
  ips <- predictX(mat, bt)
  ps <- exp(ips)
  ps/sum(ps)
}

mcm_sgd <- function(mats, choice, bt = NULL, l1p = 0.1, l2p = 0, eps = 0.1,
                    weights = rep(1, length(mats))) {
  n <- length(mats)
  if (is.null(bt)) {
    mat <- mats[[1]]
    bt <- numeric(ncolsX(ncol(mat)))
  }
  for (i in 1:n) {
    ww <- weights[i]
    xx <- mats[[i]]
    y <- choice[i]
    ps <- mcm_probs(xx, bt)
    ps[y] <- ps[y] - 1
    bt <- gradientX(bt, xx, -ww * eps * ps)
    bt <- shrinker(bt, eps * l1p, eps * l2p)
  }
  bt
}

mcm_loss <- function(mats, choice, legals, bt) {
  n <- length(mats)
  probs <- numeric(n)
  corrects <- numeric(n)
  pmat <- matrix(NA, n, length(allmoves)); colnames(pmat) <- allmoves
  for (i in 1:n) {
    xx <- mats[[i]]
    mvs <- legals[[i]]
    y <- choice[i]
    ps <- mcm_probs(xx, bt)
    pmat[i, match(mvs, allmoves)] <- ps
    if (which(ps == max(ps))[1] == y) corrects[i] <- 1
    probs[i] <- ps[y]
  }
  list(acc = sum(corrects)/n, likloss = sum(log(probs)), 
       probs = probs, corrects = corrects, pmat = pmat)
}


