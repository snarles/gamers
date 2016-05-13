mseekS <- 20
mavoidS <- 20
mseekG <- 20
mavoidG <- 20

next_move <- function(state) {
  #raw <- solve_state_raw(state, TRUE)
  if (state[4] %% 2 == 1) {
    mseek <- mseekG; mavoid <- mavoidG
  } else {
    mseek <- mseekS; mavoid <- mavoidS
  }
  alt <- getAlt(state)
  raw <- solve_state_raw(state)
  res <- analysis_to_values(raw)
  alt <- res[[1]]
  mn <- alt[-1, 1:2, drop = FALSE]
  if (state[4] %% 2 == 1) mn[, 1] <- -mn[, 1]
  filt1 <- mn[, 1]==1 & mn[, 2] < mseek
  if (sum(filt1) > 0) {
    mvs <- rownames(mn)[filt1]
    print(mn[filt1, , drop = FALSE])
    return(sample(mvs, 1))
  }
  filt2 <- (mn[, 1]==0) | (mn[, 1]==1) | (mn[, 1]==-1 & mn[, 2] > mavoid)
  if (sum(filt2) > 0) {
    mvs <- rownames(mn)[filt2]
    print(mn[filt2, , drop = FALSE])
    return(sample(mvs, 1))
  }
  ind <- order(-mn[, 2])[1]
  return(rownames(mn)[ind])
}
