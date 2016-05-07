Rcpp::sourceCpp("doubutsu1/prediction/interaction.cpp")

MN_MAX_LEN <- 120
MN_BT_LEN <- 2 * (MN_MAX_LEN + 1)

eye_mn <- pracma::eye(MN_MAX_LEN)
expand_mn <- function(mat) {
  ans <- matrix(0, nrow(mat), 2 + 2 * MN_MAX_LEN)
  ans[, 1] <- (mat[, 1]==1)
  ans[, 2] <- (mat[, 1]==-1)
  vs <- mat[, 2]
  filt <- (mat[, 1]==1) & (vs > 0) & (vs <= MN_MAX_LEN)
  ans[filt, 2 + (1:MN_MAX_LEN)] <- eye_mn[vs[filt], , drop = FALSE]
  filt <- (mat[, 1]==-1) & (vs > 0) & (vs <= MN_MAX_LEN)
  ans[filt, 2 + MN_MAX_LEN + (1:MN_MAX_LEN)] <- eye_mn[vs[filt], , drop = FALSE]
  rownames(ans) <- rownames(mat)
  colnames(ans) <- c("win", "lose", 
                     paste0("w", 1:MN_MAX_LEN),
                     paste0("l", 1:MN_MAX_LEN))
  ans
}

mn_probs <- function(mat, bt) {
  ips <- predict1(mat, bt)
  ps <- exp(ips)
  ps/sum(ps)
}

mn_sgd <- function(mats, bt = NULL, l1p = 0.1, l2p = 0, eps = 0.1,
                    weights = rep(1, length(mats))) {
  n <- length(mats)
  if (is.null(bt)) {
    bt <- numeric(MN_BT_LEN)
  }
  for (i in 1:n) {
    ww <- weights[i]
    xx <- expand_mn(mats[[i]])
    y <- 1
    ps <- mn_probs(xx, bt)
    ps[y] <- ps[y] - 1
    bt <- gradient1(bt, xx, -ww * eps * ps)
    bt <- shrinker(bt, eps * l1p, eps * l2p)
  }
  bt
}

mn_loss <- function(mats, bt) {
  n <- length(mats)
  corrects <- numeric(n)
  probs <- numeric(n)
  if (is.null(bt)) {
    bt <- numeric(MN_BT_LEN)
  }
  for (i in 1:n) {
    xx <- expand_mn(mats[[i]])
    y <- 1
    ps <- mn_probs(xx, bt)
    probs[i] <- ps[y]
    corrects[i] <- (order(-ps)[1] == y)
  }
  list(acc = sum(corrects)/n, likloss = sum(log(probs)), 
       probs = probs, corrects = corrects)
}

smooth_mn <- function(mn) {
  winv <- mn[2 + (1:MN_MAX_LEN)]
  losev <- mn[2 + MN_MAX_LEN + (1:MN_MAX_LEN)]
  res1 <- isoreg(which(winv != 0), -winv[winv != 0])
  winv2 <- winv
  winv2[winv != 0] <- -res1$yf
  res2 <- isoreg(which(losev != 0), losev[losev != 0])
  losev2 <- losev
  losev2[losev != 0] <- res2$yf
  # plot(winv, type = "l"); lines(winv2, col ="red")
  # plot(losev, type = "l"); lines(losev2, col ="red")
  c(mn[1:2], winv2, losev2)
}
