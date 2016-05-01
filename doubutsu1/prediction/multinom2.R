####
##  Multinomial prediction with interactions
####

source("doubutsu1/lg_analysis_setup.R")
# Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")
# Rcpp::sourceCpp("doubutsu1/prediction/interaction3.cpp")  ## use for order-3!!!

eye11 <- pracma::eye(11)
ut3 <- pracma::eye(3); ut3[upper.tri(ut3)] <- 1

expand_state <- function(state) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  ptypes <- board[1, ] + board[2, ] * 5 + board[3, ]
  ptypes[board[1, ]==0] <- 0
  bboard <- as.numeric(eye11[, ptypes + 1][-1, ])
  names(bboard) <- paste(rep(c("K", "R", "B", "P", "T", "k", "r", "b", "p", "t"), 12), 
                         rep(1:12, each = 10), sep = "")
  h1 <- as.numeric(ut3[, hand1 + 1][-1, ])
  h2 <- as.numeric(ut3[, hand2 + 1][-1, ])
  names(h1) <- paste(rep(c("K", "R", "B", "P"), each = 2), rep(c("x1", "x2")), sep = "")
  names(h2) <- paste(rep(c("K", "R", "B", "P"), each = 2), rep(c("x1", "x2")), sep = "")
  ans <- c(bboard, h1, h2)
  ## length(ans) ## 136
  ans
}



mcm_probs <- function(mat, bt) {
  ips <- predictX(mat, bt)
  ps <- exp(ips)
  ps/sum(ps)
}

mcm_sgd <- function(mats, choice, bt = NULL, l1p = 0.1, l2p = 0, eps = 0.1) {
  n <- length(mats)
  if (is.null(bt)) {
    mat <- mats[[1]]
    bt <- numeric(ncolsX(ncol(mat)))
  }
  for (i in 1:n) {
    xx <- mats[[i]]
    y <- choice[i]
    ps <- mcm_probs(xx, bt)
    ps[y] <- ps[y] - 1
    bt <- gradientX(bt, xx, -eps * ps)
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


