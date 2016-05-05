
source("doubutsu1/set_up_vs_cpu.R")

winners <- c()
ress <- list()

layout(t(1:2))
draw_state(init_state)
sink("temp.txt")
res <- cvc(NULL, ai_moveE, bs, bg, next_move_from_ai, bs, bg, nsample = 1, mateXdepth = 5, plotit = TRUE)
ress <- c(ress, list(res))
winners <- c(winners, res$winner)
sink()
# tt <- statesFromGame(res$mlist, printt=TRUE)
