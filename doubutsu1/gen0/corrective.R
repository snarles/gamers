source("doubutsu1/gen0/sourceE.R")
source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/viz.R")

analyze_state <- function(state, bs, bg, nsample = 1, mateXdepth = 5, nreps = 2, move.limit = 10,
                          verbose = FALSE) {
  tree <- build_tree(state, 1, 200)
  pl <- c("sente", "gote")[state[4] %% 2 + 1]
  alts <- tree[-1, , drop = FALSE]
  (na <- nrow(alts))
  vals <- numeric(na)
  mXs <- numeric(na)
  mvs <- legal_moves(state, tree)
  names(vals) <- mvs
  for (j in 1:na) {
    mX <- mateX(alts[j, ], mateXdepth + 1)
    mXs[j] <- mX
    if (is.na(mX)) {
      for (r in 1:nreps) {
        res <- selfplay(NULL, ai_moveE, bs, bg, nsample, mateXdepth, start = alts[j, ], move.limit)
        if (res$winner == "") {
          vals[j] <- vals[j] + 0.5/nreps
        } else {
          vals[j] <- vals[j] + (res$winner == pl)/nreps
        }
      }
    } else {
      if (mX > 0) vals[j] <- 0
      if (mX <= 0) vals[j] <- 1
    }
  }
  if (verbose) {
    draw_state(state)
    title(paste(pl, "to move"), 
          sub = paste(paste(names(vals)[which(vals==max(vals))], collapse =", "), max(vals), sep = " = "))
  }
  print(sort(vals, TRUE))
  print(mXs)
  alts[, 1] <- vals
  rownames(alts) <- mvs
  names(mXs) <- mvs
  if (max(vals) == 0) vals[is.na(mXs)] <- vals[is.na(mXs)] + 0.001
  cands <- names(vals)[vals == max(vals)]
  if (sum(is.na(mXs))==0 && min(mXs) > 0) cands <- "resign"
  list(pos = state, alts = alts, vals = vals, cands = cands)
}


# state <- states[sample(nrow(states), 1), ]
# resA <- analyze_state(state, bs, bg, verbose = TRUE)
# res <- selfplay(NULL, ai_moveE, bs, bg, nsample, mateXdepth, start = state)
# res$winner
