


selfplay <- function(seed = 0, Bs, Bg, nsample, mateXdepth) {
  set.seed(seed)  
  slist <- list()
  state <- init_state
  mlist <- character()
  mv <- ai_move(state, Bs, Bg, nsample, mateXdepth)
  mlist <- c(mlist, mv)
  while (mv != "resign") {
    state <- move_parser(state, mv)
    slist <- c(slist, list(state))
    print_state(state)
    mv <- ai_move(state, Bs, Bg, nsample, mateXdepth)
    mlist <- c(mlist, mv)
  }
  return(list(mlist = mlist, slist = slist, seed = seed, nsample = nsample, mateXdepth = mateXdepth))
}

