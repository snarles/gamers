# source("doubutsu1/prediction/multinom2.R")
source("doubutsu1/source.R")
sourceCpp("doubutsu1/Rsource.cpp")
sourceCpp("doubutsu1/Rsource2.cpp")
sourceCpp("doubutsu1/prediction/interaction2a.cpp")


mcm_probs <- function(mat, bt) {
  ips <- predictX(mat, bt)
  ps <- exp(ips)
  ps/sum(ps)
}

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

legal_preds <- function(preds, legals) {
  mpreds <- character()
  for (i in 1:nrow(preds)) {
    inds <- intersect(legals[[i]], colnames(preds))
    if (length(inds) == 0) {
      mpreds[i] <- "unknown"
    } else {
      pvs <- preds[i, inds]
      mpreds[i] <- inds[pvs == max(pvs)][1]
    }
  }
  mpreds
}

movestr <- function(movev) {
  paste(PIECESTRS[movev[1] + 1, 1],
        c("**", LOX)[movev[2] + 1],
        c("**", LOX)[movev[3] + 1],
        sep = "-")
}

legal_moves <- function(state, tree = build_tree(state, 1, 200)) {
  mvs <- apply(tree[-1, 49:51, drop = FALSE], 1, movestr)
  mvs
}

ai_moveE <- function(state, Bs, Bg, nsample = 3, mateXdepth = 3, verbose = TRUE) {
  if (state[4] %% 2 == 0) {
    # print("AIE_sente")
    B <- Bs
  } else {
    # print("AIE_gote")
    B <- Bg
  }
  tree <- build_tree(state, 1, nodemax = 200)
  mvs <- legal_moves(state, tree)
  s2 <- tree[-1, , drop = FALSE]
  a1 <- t(apply(s2, 1, expandState))
  ps <- mcm_probs(a1, B)
  mv <- sample(mvs, pmin(nsample, length(mvs)), FALSE, prob = ps)
  inds <- match(mv, mvs)
  vals <- 0 * inds
  for (i in 1:length(inds)) {
    vals[i] <- mateX(s2[inds[i], ], mateXdepth)
  }
  vals[is.na(vals)] <- 0; names(vals) <- mv
  if (verbose) print(vals)
  if (max(vals) < 0) return(mv[which(vals==min(vals))[1]])
  if (min(vals) > 0) return("resign")
  return(sample(mv[vals <= 0], 1))
}



next_move_mateX <- function(state, mateXdepth = 3) {
  mX <- mateX(state, mateXdepth)
  if (is.na(mX)) return("unknown")
  if (mX < 0) return("resign")
  tree <- build_tree(state, 1, 200)
  alts <- tree[-1, , drop = FALSE]
  vals <- apply(alts, 1, mateX, mateXdepth)
  mvs <- legal_moves(state, tree)
  mv <- mvs[which(vals <= 0)[1]]
  print(c(mv, "MATE-MOVE"))
  return(mv)
}

# nsample is obsoleted
ai_moveEM <- function(state, Bs, Bg, nsample = 3, mateXdepth = 3, verbose = TRUE) {
  mv <- next_move_mateX(state, mateXdepth)
  if (mv != "unknown") return(mv)
  if (state[4] %% 2 == 0) {
    # print("AIE_sente")
    B <- Bs
  } else {
    # print("AIE_gote")
    B <- Bg
  }
  tree <- build_tree(state, 1, nodemax = 200)
  mvs <- legal_moves(state, tree)
  s2 <- tree[-1, , drop = FALSE]
  a1 <- t(apply(s2, 1, expandState))
  ps <- mcm_probs(a1, B)
  inds <- sample(length(mvs), length(mvs), FALSE, prob = ps)
  flag <- TRUE
  i <- 0
  while (flag) {
    i <- i + 1
    if (i > length(inds)) {return("resign")}
    mX <- mateX(s2[inds[i], ], mateXdepth)
    if (is.na(mX)) {
      flag <- FALSE
    } else {
      #cat(mvs[inds[i]]); cat(mX)
    }
  }
 return(mvs[inds[i]])
}