####
##  Multinomial prediction with interactions
####

source("doubutsu1/lg_analysis_setup.R")
# Rcpp::sourceCpp("doubutsu1/prediction/interaction.cpp")
Rcpp::sourceCpp("doubutsu1/prediction/interaction3.cpp")  ## use for order-3!!!
alts <- readRDS("doubutsu1/altMoves.rds")

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

shrinker <- function(bt, l1p, l2p) {
  s <- sign(bt)
  filt <- abs(bt) > l1p
  bt[!filt] <- 0
  bt[filt] <- bt[filt] - s[filt] * l1p
  bt <- bt - l2p * bt
  bt
}

makeAltTable <- function(ginds) {
  senteAlts <- list()
  goteAlts <- list()
  senteChoice <- numeric()
  goteChoice <- numeric()
  for (gind in ginds) {
    game <- alts[[gind]]
    for (turn in 2:length(game)) {
      alt <- game[[turn]]
      choice <- which(alt[, 1]==1)[1]
      mat <- t(apply(alt, 1, expand_state))
      if (turn %% 2 == 1) {
        senteAlts <- c(senteAlts, list(mat))
        senteChoice <- c(senteChoice, choice)
      } else {
        goteAlts <- c(goteAlts, list(mat))
        goteChoice <- c(goteChoice, choice)
      }
    }
  }
  list(senteAlts = senteAlts, senteChoice = senteChoice,
       goteAlts = goteAlts, goteChoice = goteChoice)
}

mcm_probs <- function(mat, bt) {
  ips <- predict2(mat, bt)
  ps <- exp(ips)
  ps/sum(ps)
}

mcm_sgd <- function(mats, choice, bt = NULL, l1p = 0.1, l2p = 0, eps = 0.1) {
  n <- length(mats)
  if (is.null(bt)) {
    mat <- mats[[1]]
    bt <- numeric(ncols2(ncol(mat)))
  }
  for (i in 1:n) {
    xx <- mats[[i]]
    y <- choice[i]
    ps <- mcm_probs(xx, bt)
    ps[y] <- ps[y] - 1
    grad <- average2(xx, ps)
    bt <- bt - eps * grad
    bt <- shrinker(bt, eps * l1p, eps * l2p)
  }
  bt
}

mcm_loss <- function(mats, choice, bt, feature = ident) {
  n <- length(mats)
  probs <- numeric(n)
  corrects <- numeric(n)
  for (i in 1:n) {
    xx <- mats[[i]]
    y <- choice[i]
    ps <- mcm_probs(xx, bt)
    if (which(ps == max(ps))[1] == y) corrects[i] <- 1
    probs[i] <- ps[y]
  }
  list(acc = sum(corrects)/n, likloss = sum(log(probs)), probs = probs, corrects = corrects)
}

resTr <- makeAltTable(trinds)
resTe <- makeAltTable(teinds)
resTr$senteAlts[[1]]


mats <- resTr$senteAlts
choice <- resTr$senteChoice
bt <- mcm_sgd(mats, choice, l1p = 0.01, l2p = 0.1, eps = 0.1)
mcm_loss(mats, choice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 0.1, l2p = 0.01, eps = 0.001)
mcm_loss(mats, choice, bt)[1:2] 
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2] 


bt <- mcm_sgd(mats, choice, bt, l1p = 1e-3, l2p = 1e-4, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]

bt <- mcm_sgd(mats, choice, bt, l1p = 0, l2p = 0, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 0, l2p = 0, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 0, l2p = 0, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]

plot(bt)



bt <- mcm_sgd(mats, choice, bt, l1p = 1e-3, l2p = 1e-4, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 1e-3, l2p = 1e-4, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 1e-3, l2p = 1e-4, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]

bt <- mcm_sgd(mats, choice, bt, l1p = 1e-4, l2p = 1e-5, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 1e-4, l2p = 1e-5, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 1e-4, l2p = 1e-5, eps = 1e-3)
mcm_loss(mats, choice, bt)[1:2]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1:2]

saveRDS(bt, "doubutsu1/prediction/bt_order3sente_516.rds")


mats <- resTr$goteAlts
choice <- resTr$goteChoice
bt <- mcm_sgd(mats, choice, l1p = 0.01, l2p = 0.1, eps = 0.1)
mcm_loss(mats, choice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, l1p = 0.01, l2p = 0.1, eps = 0.0001)
mcm_loss(mats, choice, bt)[1:2]

mcm_loss(resTe$goteAlts, resTe$goteChoice, bt)[1:2]
