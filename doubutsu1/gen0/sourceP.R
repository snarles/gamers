source("doubutsu1/prediction/multiclassreg.R")
source("doubutsu1/source.R")
sourceCpp("doubutsu1/Rsource.cpp")
sourceCpp("doubutsu1/Rsource2.cpp")


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

ai_move <- function(state, Bs, Bg, nsample = 3, mateXdepth = 3) {
  if (state[4] %% 2 == 0) {
    B <- Bs
  } else {
    B <- Bg
  }
  tree <- build_tree(state, 1, nodemax = 200)
  mvs <- legal_moves(state, tree)
  s2 <- tree[-1, , drop = FALSE]
  ex <- expand_state(state)
  ps <- as.numeric(mpreds(t(ex), B))
  ps[!colnames(B) %in% mvs] <- 0
  ps <- ps/sum(ps); names(ps) <- colnames(B); sort(ps, TRUE)
  mv <- sample(colnames(B), pmin(nsample, sum(ps!=0)), FALSE, prob = ps)
  inds <- match(mv, mvs)
  vals <- 0 * inds
  for (i in 1:length(inds)) {
    vals[i] <- mateX(s2[inds[i], ], mateXdepth)
  }
  vals[is.na(vals)] <- 0; names(vals) <- mv
  print(vals)
  if (max(vals) < 0) return(mv[which(vals==min(vals))[1]])
  if (min(vals) > 0) return("resign")
  return(sample(mv[vals <= 0], 1))
}


