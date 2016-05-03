


selfplay <- function(seed = 0, ai_move, Bs, Bg, nsample, mateXdepth) {
  return(cvc(seed, ai_move, Bs, Bg, ai_move, Bs, Bg, nsample, mateXdepth))
}

cvc <- function(seed = 0, ai_move1, Bs1, Bg1, ai_move2, Bs2, Bg2, nsample, mateXdepth) {
  set.seed(seed)  
  slist <- list()
  state <- init_state
  mlist <- character()
  mv <- ai_move1(state, Bs1, Bg1, nsample, mateXdepth)
  mlist <- c(mlist, mv)
  while (mv != "resign") {
    state <- move_parser(state, mv)
    slist <- c(slist, list(state))
    print_state(state)
    if (state[4] %% 2 == 0) {
      mv <- ai_move1(state, Bs1, Bg1, nsample, mateXdepth)
    } else {
      mv <- ai_move2(state, Bs2, Bg2, nsample, mateXdepth)
    }
    mlist <- c(mlist, mv)
  }
  return(list(mlist = mlist, slist = slist, seed = seed, nsample = nsample, mateXdepth = mateXdepth))
}
