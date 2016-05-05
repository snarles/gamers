
source("doubutsu1/set_up_vs_cpu.R")

winners <- c()
ress <- list()

res <- cvc(NULL, ai_moveE, bs, bg, next_move_from_ai, bs, bg, nsample = 1, mateXdepth = 5)
ress <- c(ress, list(res))
winners <- c(winners, res$winner)
tt <- statesFromGame(res$mlist, printt=TRUE)
