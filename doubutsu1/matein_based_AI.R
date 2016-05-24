val_in_env <- function(var, def) {
  if (var %in% ls()) {
    return(.GlobalEnv[[var]])
  }
  def
}

mseekS <- val_in_env("mseekS", 20)
mavoidS <- val_in_env("mavoidS", 20)
mseekG <- val_in_env("mseekG", 20)
mavoidG <- val_in_env("mavoidG", 20)

next_move <- function(state, mseekS = mseekS, mavoidS = mavoidS, mseekG = mseekG, mavoidG = mavoidG) {
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

## resigns if less than seek value
next_move2 <- function(state, mseekS = mseekS, mavoidS = mavoidS, mseekG = mseekG, mavoidG = mavoidG) {
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
  return("resign")
}

get_sp <- function(seek_val = 20, state = init_state) {
  mv <- ""
  game <- character()
  while (mv != "resign") {
    mv <- next_move2(state, seek_val, seek_val, seek_val, seek_val)
    game <- c(game, mv)
    state <- move_parser(state, mv)
  }
  return(game)
}

# svs <- rep(10 * (1:5), each = 100)
# res <- lapply(svs, get_sp)