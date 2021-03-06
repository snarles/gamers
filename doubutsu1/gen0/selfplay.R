


selfplay <- function(seed = 0, ai_move, Bs, Bg, nsample, mateXdepth, start = init_state, 
                     move.limit = 100, verbose = TRUE) {
  return(cvc(seed, ai_move, Bs, Bg, ai_move, Bs, Bg, nsample, mateXdepth, start, move.limit, verbose))
}

selfplay2 <- function(seed = 0, ai_move, Bs, Bg, Bse, Bge, nsample, mateXdepth, expo) {
  return(cvc2(seed, ai_move, Bs, Bg, Bse, Bge, ai_move, Bs, Bg, Bse, Bge, nsample, mateXdepth, expo))
}

cvc <- function(seed = 0, ai_move1, Bs1, Bg1, ai_move2, Bs2, Bg2, nsample, mateXdepth, start = init_state,
                move.limit = 100, verbose = TRUE, plotit = FALSE) {
  if (!is.null(seed)) set.seed(seed)  
  state <- start
  slist <- list(state)
  if (verbose) print_state(state)
  mlist <- character()
  mv <- ai_move1(state, Bs1, Bg1, nsample, mateXdepth)
  mlist <- c(mlist, mv)
  winner = ""
  mvct <- 0
  while (mv != "resign" && mvct < move.limit) {
    state <- move_parser(state, mv)
    slist <- c(slist, list(state))
    if (verbose) print_state(state)
    if (plotit) draw_state(state, title = TRUE)
    if (state[4] %% 2 == 0) {
      mv <- ai_move1(state, Bs1, Bg1, nsample, mateXdepth)
    } else {
      mv <- ai_move2(state, Bs2, Bg2, nsample, mateXdepth)
    }
    mlist <- c(mlist, mv)
    mvct <- mvct + 1
  }
  if (mvct < move.limit) {
    if (state[4] %% 2 == 0) {
      winner = "gote"
    } else {
      winner = "sente"
    }
  }
  return(list(mlist = mlist, slist = slist, seed = seed, nsample = nsample, mateXdepth = mateXdepth,
              winner = winner))
}


cvc2 <- function(seed = 0, ai_move1, Bs1, Bg1, Bse1, Bge1, 
                 ai_move2, Bs2, Bg2, Bse2, Bge2, nsample, mateXdepth, expo) {
  set.seed(seed)  
  slist <- list()
  state <- init_state
  mlist <- character()
  mv <- ai_move1(state, Bs1, Bg1, Bse1, Bge1, nsample, mateXdepth, expo)
  mlist <- c(mlist, mv)
  while (mv != "resign") {
    state <- move_parser(state, mv)
    slist <- c(slist, list(state))
    print_state(state)
    if (state[4] %% 2 == 0) {
      mv <- ai_move1(state, Bs1, Bg1, Bse1, Bge1, nsample, mateXdepth, expo)
    } else {
      mv <- ai_move2(state, Bs2, Bg2, Bse2, Bge2, nsample, mateXdepth, expo)
    }
    mlist <- c(mlist, mv)
  }
  return(list(mlist = mlist, slist = slist, seed = seed, nsample = nsample, mateXdepth = mateXdepth, expo = expo))
}
