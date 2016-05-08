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

SPECTATOR_T <- 1

library(Rcpp)
# load("doubutsu1/vs_cpu.rda")
source("doubutsu1/set_up_vs_cpu.R")
sourceCpp("doubutsu1/Rsource.cpp")
sourceCpp("doubutsu1/Rsource2.cpp")
source("doubutsu1/viz.R")
source("doubutsu1/gui.R")

par(mar = c(0, 0, 1, 0))
forget_book_prob <- 0.2
#mnBt <- smooth_mn(mnBt)
multBt <- 1

layout(t(matrix(1:9, 3, 3)))
game_records <- list()
new_alts <- list()
winners <- c()

alt_summary <- function(alt) {
  list(uh = uhash2(alt[1, ]), mn = alt[-1, 1:2, drop = FALSE])
}


t1 <- proc.time()
for (iii in 1:100) {
  game_record <- character()
  game_states <- list()
  state <- init_state
  flag <- TRUE
  while (flag) {
    new_alt <- list()
    #draw_state(state, title = TRUE)
    print_state(state)
    Sys.sleep(SPECTATOR_T)
    sink("temp.txt")
    mv <- next_move(state)
    sink()
    if (mv == "resign") {
      print_state(state)
      draw_state(state, title = FALSE)
      pl <- state[4] %% 2
      if (pl == 0) {
        title("Sente resigns!")
        winner <- "gote"
      }
      if (pl == 1) {
        title("Gote resigns!!")
        winner <- "sente"
      }
      flag <- FALSE
      winners <- c(winners, winner)
    }
    game_record <- c(game_record, mv)
    state <- move_parser(state, mv)
    game_states <- c(game_states, list(state))
    mX <- mateX(state, 2)
    if (!is.na(mX) && mX <= 0) {
      print_state(state)
      draw_state(state, title = TRUE)
      flag <- FALSE
      pl <- state[4] %% 2
      if (pl == 0 && state[45]==1) winner <- "gote"
      if (pl == 1 && state[41]==1) winner <- "sente"
      if (pl == 0 && state[45]!=1) winner <- "gote"
      if (pl == 1 && state[41]!=1) winner <- "sente"
      title(sub = winner)
      winners <- c(winners, winner)
    }
    new_alts <- c(new_alts, lapply(new_alt, alt_summary))
    game_records <- c(game_records, list(game_record))
  }
  print(table(winners))
  print("NEW GAME WILL BEGIN in 10s...")
  Sys.sleep(9 * SPECTATOR_T + 1)
}
proc.time() - t1









altu <- sapply(new_alts, function(v) uhash2(v[1, ]))
oldu <- readRDS("doubutsu1/old_altu.rds")
ua <- setdiff(unique(altu), oldu)
length(ua)
if (length(ua) > 0) {
  new_alts <- new_alts[match(unique(altu), altu)]
  length(new_alts)
  saveRDS(new_alts, "doubutsu1/temp_new_alts.rds")
}
saveRDS(game_records, "doubutsu1/temp_records.rds")

