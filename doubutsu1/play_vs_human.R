cat("Loading...")
cat("\n")

library(Rcpp)
# load("doubutsu1/vs_cpu.rda")
source("doubutsu1/set_up_vs_cpu.R")
sourceCpp("doubutsu1/Rsource.cpp")
sourceCpp("doubutsu1/Rsource2.cpp")
source("doubutsu1/viz2.R")
source("doubutsu1/gui.R")

mv <- ""
state <- init_state
flag <- TRUE
while (mv != "resign" && flag) {
  draw_state(state, title = TRUE)
  mv <- query_move(state)
  state <- move_parser(state, mv)
  print(maxVal(state, 0))
}