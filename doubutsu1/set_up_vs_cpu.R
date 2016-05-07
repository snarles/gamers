source("doubutsu1/gen0/sourceP.R")
source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/viz.R")
source("doubutsu1/gen0/sourceE.R")
bs <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bg <- readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
source("doubutsu1/gen0/corrective.R")
load("doubutsu1/prediction/mn00.rds")
# mnBt <- smooth_mn((mnS + mnG)/2)
mnBt <- (mnS + mnG)/2
source("doubutsu1/sourceJP.R")

nsample = 1; mateXdepth = 5
games <- list()
move.limit <- 3

database <- readRDS("doubutsu1/lg_states.rds")
database <- database[sapply(database, length) > 2]
database <- lapply(database, do.call, what = rbind)
database <- do.call(rbind, database)
hashes <- apply(database, 1, hash_state)
statepl <- database[, 4] %% 2

# overwite this in specific code?
next_move <- function(state) {
  mv <- next_move_mateX(state, 3)
  if (mv != "unknown") return(mv)
  mv <- next_move_from_book(state)
  if (mv != "unknown") return(mv)
  next_move_from_ai(state, bg, bs, 1, 5, 4, 10, FALSE)
}

opening_move_from_book <- function() {
  filt <- database[, 4] == 1
  ind <- sample(which(filt), 1)
  st <- database[ind, ]
  mv <- movestr(st[49:51])
  Sys.sleep(0.2)
  return(mv)
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
  Sys.sleep(0.5)
  return(mv)
}

next_move_from_book <- function(state) {
  pl <- state[4] %% 2
  h <- hash_state(state)
  filt <- (hashes == h) & (statepl == pl)
  ind <- which(filt)
  if (length(ind) == 0) return("unknown")
  if (length(ind) > 1) ind <- sample(ind, 1)
  if (database[ind, 4] == (database[ind + 1, 4] - 1) ) {
    mv <- movestr(database[ind+1, 49:51])
    print(c(mv, "LG-(HUMAN)-MOVE"))
    mvs <- legal_moves(state)
    stopifnot(mv %in% mvs)
    Sys.sleep(0.5)
    return(mv)
  }
  return ("unknown")
}

# next_move_from_ai <- function(state, ...) {
#   res <- analyze_state(state, ...)
#   mv <- sample(res$cands, 1)
#   print(c(mv, "AI MOVE"))
#   return(mv)
# }

next_move_from_ai <- function(state, ...) {
  #raw <- solve_state_raw(state, TRUE)
  alt <- getAlt(state)
  X <- t(apply(alt, 1, expandState))
  if (state[4] %% 2 == 0) {
    ps <- predictX(X, bs)
  } else {
    ps <- predictX(X, bg)
  }
  ps <- exp(ps)/sum(exp(ps)); names(ps) <- rownames(alt)
  raw <- solve_state_raw(state)
  res <- analysis_to_values(raw)
  alt <- res[[1]]
  mn <- alt[-1, 1:2, drop = FALSE]
  ps2 <- mn_probs(expand_mn(mn), mnBt)
  ps2[mn[, 1]==-1 & mn[,2]==0] <- 0; ps2 <- ps2/sum(ps2)
  mvs <- sample(names(ps2), 3, TRUE, prob = ps2)
  stopifnot(all(names(mvs) %in% names(ps)))
  ps3 <- ps[mvs]; ps3 <- ps3/sum(ps3)
  ind <- sample(length(ps3), 1, prob = ps3)
  mv <- names(ps3)[ind]
  print(list(msg = "AI_MOVE", mn = mn, ps = ps, ps2 = ps2, ps3 = ps3, mv))
  return(mv)
}


# next_move_from_ai(state)

# save.image(file = "doubutsu1/vs_cpu.rda")

