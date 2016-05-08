####
##  CPU vs itself
####
# > alts <- readRDS("doubutsu1/solved_alts2.rds")
# > altu <- sapply(alts, function(v) uhash2(v[1, ]))
# > length(altu)
# [1] 49225
# > length(unique(altu))
# [1] 49225
# > saveRDS(altu, "doubutsu1/old_altu.rds")

cat("Loading...")
cat("\n")


library(Rcpp)
# load("doubutsu1/vs_cpu.rda")
source("doubutsu1/set_up_vs_cpu.R")
sourceCpp("doubutsu1/Rsource.cpp")
sourceCpp("doubutsu1/Rsource2.cpp")
source("doubutsu1/viz.R")
source("doubutsu1/gui.R")

par(mar = c(0, 0, 1, 0))
forget_book_prob <- 0.8
multBt <- 1

layout(t(matrix(1:9, 3, 3)))
game_records <- list()
new_alts <- list()
winners <- c()

for (iii in 1:100) {
  game_record <- character()
  game_states <- list()
  state <- init_state
  flag <- TRUE
  while (flag) {
    new_alt <- list()
    draw_state(state, title = TRUE)
    mv <- next_move(state)
    game_record <- c(game_record, mv)
    state <- move_parser(state, mv)
    game_states <- c(game_states, list(state))
    mX <- mateX(state, 2)
    if (!is.na(mX) && mX <= 0) {
      draw_state(state, title = FALSE)
      flag <- FALSE
      pl <- state[4] %% 2
      if (pl == 0 && state[45]==1) winner <- "gote"
      if (pl == 1 && state[41]==1) winner <- "sente"
      if (pl == 0 && state[45]!=1) winner <- "gote"
      if (pl == 1 && state[41]!=1) winner <- "sente"
      title(winner)
      winners <- c(winners, winner)
    }
    new_alts <- c(new_alts, new_alt)
  }
  print("NEW GAME WILL BEGIN in 10s...")
  Sys.sleep(10)
}
table(winners)
altu <- sapply(new_alts, function(v) uhash2(v[1, ]))
oldu <- readRDS("doubutsu1/old_altu.rds")
ua <- setdiff(unique(altu), oldu)
length(ua)
if (length(ua) > 0) {
  new_alts <- new_alts[match(unique(altu), altu)]
  length(new_alts)
  saveRDS(new_alts, "doubutsu1/temp_new_alts.rds")
}
