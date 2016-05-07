
source("doubutsu1/prediction/multinom2.R")

Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")

mcm_probs <- function(mat, bt) {
  ips <- predictX(mat, bt)
  ps <- exp(ips)
  ps/sum(ps)
}

mcm_sgd <- function(mats, bt = NULL, l1p = 0, l2p = 0, eps = 0.1) {
  n <- length(mats)
  if (is.null(bt)) {
    bt <- numeric(ncolsX(136))
  }
  for (i in 1:n) {
    xx <- t(apply(mats[[i]], 1, expandState))
    ps <- mcm_probs(xx, bt)
    ps <- ps - xx[, 1]
    bt <- gradientX(bt, xx, -eps * ps)
    bt <- shrinker(bt, eps * l1p, eps * l2p)
  }
  bt
}

mcm_loss <- function(mats, bt) {
  n <- length(mats)
  accs <- numeric(n)
  for (i in 1:n) {
    xx <- t(apply(mats[[i]], 1, expandState))
    ps <- mcm_probs(xx, bt)
    ep <- sum(ps * (xx[, 1] > 0))
    accs[i] <- ep
  }
  list(me = mean(accs), accs = accs)
}

convert_to_alt <- function(mat) {
  pl <- mat[1, 4] %% 2
  mat <- mat[-1, , drop = FALSE]
  if (pl == 1) mat[, 1] <- -mat[, 1]
  vv <- mat[, 1]/(0.01+mat[, 2])
  mv <- max(vv)
  inds <- which(vv >= mv/2 | vv >= 2 * mv)
  mat[, 1] <- 0
  mat[inds, 1] <- 1/length(inds)
  mat
}


alts <- readRDS("doubutsu1/solved_alts.rds")
alts <- do.call(c, alts)
senteS <- sapply(alts, function(v) v[1, 4])
alts <- lapply(alts, convert_to_alt)
altsS <- alts[senteS==0]
altsG <- alts[senteS==1]
rm(alts); gc()
filtS <- rbinom(length(altsS), 1, 0.5)
filtG <- rbinom(length(altsG), 1, 0.5)
altsStr <- altsS[filtS==1]
altsSte <- altsS[filtS==0]
altsGtr <- altsG[filtG==1]
altsGte <- altsG[filtG==0]
rm(altsS); rm(altsG); gc()


btS <- mcm_sgd(altsStr[1:10], eps = 0.01)
mcm_loss(altsStr[1:10], btS)[1]
