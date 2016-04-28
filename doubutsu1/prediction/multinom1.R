####
##  Multinomial prediction
####

source("doubutsu1/lg_analysis_setup.R")
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

ident <- function(v) v

## makes interaction vector
ixn2 <- function(v) {
  p <- length(v)
  nv <- names(v)
  ix2 <- v %x% v
  names(ix2) <- paste(rep(nv, each = p), ".", rep(nv, p), sep = "")
  ## remove self-interactions
  rmset <- paste(nv, ".", nv, sep = "")
  ix2 <- ix2[!names(ix2) %in% rmset]
  c(v, ix2)
}

shrinker <- function(bt, type, pen) {
  if (type ==1) {
    s <- sign(bt)
    filt <- abs(bt) > pen
    bt[!filt] <- 0
    bt[filt] <- bt[filt] - s[filt] * pen
  }
  if (type == 2) {
    bt <- bt - pen * bt
  }
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

mcm_probs <- function(mat, bt, feature = ident) {
 # mat <- t(apply(mat, 1, feature))
  ips <- mat %*% bt
  ps <- exp(ips)
  ps/sum(ps)
}

mcm_sgd <- function(mats, choice, bt = NULL, penalty_type = 1, pen = 0.1, eps = 0.1, feature = ident) {
  n <- length(mats)
  if (length(eps)==1) eps <- rep(eps, n)
  if (is.null(bt)) {
    mat <- mats[[1]]
   # mat <- t(apply(mat, 1, feature))
    bt <- numeric(ncol(mat))
  }
  for (i in 1:n) {
    xx <- mats[[i]]
 #   xx <- t(apply(xx, 1, feature))
    y <- choice[i]
    ps <- mcm_probs(xx, bt)
    bt <- bt + eps[i] * (xx[y, ] - as.numeric(t(ps) %*% xx))
    bt <- shrinker(bt, penalty_type, eps[i] * pen)
  }
  bt
}

mcm_loss <- function(mats, choice, bt, feature = ident) {
  n <- length(mats)
  probs <- numeric(n)
  corrects <- numeric(n)
  for (i in 1:n) {
    xx <- mats[[i]]
   # xx <- t(apply(xx, 1, feature))
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
bt <- mcm_sgd(mats, choice, penalty_type = 1, pen = 0.1, eps = 0.1)
mcm_loss(mats, choice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, penalty_type = 1, pen = 0.01, eps = 0.1)
mcm_loss(mats, choice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, penalty_type = 2, pen = 0.001, eps = 0.0004) # 0.37
mcm_loss(mats, choice, bt)[1:2]

mats <- resTr$goteAlts
choice <- resTr$goteChoice
bt <- mcm_sgd(mats, choice, penalty_type = 1, pen = 0.1, eps = 0.1)
mcm_loss(mats, choice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, penalty_type = 1, pen = 0.01, eps = 0.1)
mcm_loss(mats, choice, bt)[1:2]
bt <- mcm_sgd(mats, choice, bt, penalty_type = 2, pen = 0.001, eps = 0.0004)  ## 0.37
mcm_loss(mats, choice, bt)[1:2]


# 
# ##  Try with 2-interactions
# bt <- mcm_sgd(mats, choice, penalty_type = 1, pen = 0.1, eps = 0.1, feature = ixn2)
# mcm_loss(mats, choice, bt, feature = ixn2)[1:2]
# bt <- mcm_sgd(mats, choice, bt, penalty_type = 1, pen = 0.01, eps = 0.1, feature = ixn2)
# mcm_loss(mats, choice, bt, feature = ixn2)[1:2]
