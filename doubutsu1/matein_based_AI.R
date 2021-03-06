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

next_move <- function(state, mseekS1 = mseekS, mavoidS1 = mavoidS, 
                      mseekG1 = mseekG, mavoidG1 = mavoidG) {
  #raw <- solve_state_raw(state, TRUE)
  if (state[4] %% 2 == 1) {
    mseek <- mseekG1; mavoid <- mavoidG1
  } else {
    mseek <- mseekS1; mavoid <- mavoidS1
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
## avoids repeats if possible
next_move2 <- function(state, mseekS1 = mseekS, mavoidS1 = mavoidS, 
                       mseekG1 = mseekG, mavoidG1 = mavoidG,
                       prevs = character()) {
  #raw <- solve_state_raw(state, TRUE)
  if (state[4] %% 2 == 1) {
    mseek <- mseekG1; mavoid <- mavoidG1
  } else {
    mseek <- mseekS1; mavoid <- mavoidS1
  }
  alt <- getAlt(state)
  raw <- solve_state_raw(state)
  res <- analysis_to_values(raw)
  alt <- res[[1]]
  mn <- alt[-1, 1:2, drop = FALSE]
  uh_new <- apply(alt[-1, , drop = FALSE], 1, uhash2)
  nonrep_filt <- !(uh_new %in% prevs)
  if (sum(nonrep_filt) == 0) nonrep_filt <- rep(TRUE, length(nonrep_filt))
  if (state[4] %% 2 == 1) mn[, 1] <- -mn[, 1]
  filt1 <- mn[, 1]==1 & mn[, 2] < mseek & nonrep_filt
  if (sum(filt1) > 0) {
    mvs <- rownames(mn)[filt1]
    print(mn[filt1, , drop = FALSE])
    return(sample(mvs, 1))
  }
  filt2 <- (mn[, 1]==0) | (mn[, 1]==1) | (mn[, 1]==-1 & mn[, 2] > mavoid) &
    nonrep_filt
  if (sum(filt2) > 0) {
    mvs <- rownames(mn)[filt2]
    print(mn[filt2, , drop = FALSE])
    return(sample(mvs, 1))
  }
  return("resign")
}

get_sp <- function(seek_val = 20, state = init_state, move.limit = 200,
                   return.uhs = FALSE) {
  mv <- ""
  game <- character()
  uhs <- character()
  flag <- TRUE
  while (flag) {
    mv <- next_move2(state, seek_val, seek_val, seek_val, seek_val, uhs)
    game <- c(game, mv)
    state <- move_parser(state, mv)
    uh <- uhash2(state)
    if (mv == "resign") flag <- FALSE
    if (length(game) > move.limit) flag <- FALSE
    if (uh %in% uhs) flag <- FALSE
    uhs <- c(uhs, uh)
  }
  if (return.uhs) {
    names(uhs) <- paste(1:length(game), game, sep = ".")
    return(uhs)
  }
  return(game)
}
