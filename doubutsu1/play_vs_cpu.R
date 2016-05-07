####
##  Play vs the cpu, blind mode
####

cat("Loading...")
cat("\n")

library(Rcpp)
# load("doubutsu1/vs_cpu.rda")
source("doubutsu1/set_up_vs_cpu.R")
sourceCpp("doubutsu1/Rsource.cpp")
sourceCpp("doubutsu1/Rsource2.cpp")
source("doubutsu1/viz2.R")
source("doubutsu1/gui.R")

game_record <- character()
game_states <- list()

diagnostic_mode <- TRUE
if (diagnostic_mode) sink("temp.txt")

draw_board(FALSE)
title("Choose player")
draw_piece(2, 1, 1, 0, 0)
draw_piece(2, 4, 1, 1, 0)
click <- locator(1)
if (click$y > 2) {
  pl <- "gote"
} else {
  pl <- "sente"
}

draw_state(state, title = TRUE)

if (pl %in% c("gote", "sente")) {
  mv <- ""
  flag <- TRUE
  state <- init_state
  game_states <- c(game_states, list(state))
  if (pl == "gote") {
    print("AI THINKING...")
    mv <- opening_move_from_book()
    game_record <- c(game_record, mv)
    state <- move_parser(state, mv)
    game_states <- c(game_states, list(state))
  }
  while(mv != "resign" && flag) {
    mv <- query_move(state)
    game_record <- c(game_record, mv)
    state <- move_parser(state, mv)
    game_states <- c(game_states, list(state))
    draw_state(state, title = TRUE)
    mX <- mateX(state, 2)
    # print(list(mX = mX, pl = state[4] %%2))
    if (mv == "resign") {
      draw_state(state, title = FALSE)
      pl <- state[4] %% 2
      if (pl == 0) title("Sente resigns!")
      if (pl == 1) title("Gote resigns!!")
    } else if (!is.na(mX) && mX <= 0) {
      draw_state(state, title = FALSE)
      flag <- FALSE
      pl <- state[4] %% 2
      if (pl == 0 && state[45]==1) title("Gote wins!")
      if (pl == 1 && state[41]==1) title("Sente wins!")
      if (pl == 0 && state[45]!=1) title("Gote wins!")
      if (pl == 1 && state[41]!=1) title("Sente wins!")
    } else {
      print("AI THINKING...")
      if (!diagnostic_mode) sink("temp.txt")
      mv <- next_move(state)
      if (!diagnostic_mode) sink()
      game_record <- c(game_record, mv)
      state <- move_parser(state, mv)
      game_states <- c(game_states, list(state))
    }
    if (mv == "resign") {
      draw_state(state, title = FALSE)
      pl <- state[4] %% 2
      if (pl == 0) title("Sente resigns!")
      if (pl == 1) title("Gote resigns!!")
    } else {
      mX <- mateX(state, 2)
      if (!is.na(mX) && mX <= 0) {
        draw_state(state, title = FALSE)
        flag <- FALSE
        pl <- state[4] %% 2
        if (pl == 0 && state[45]==1) title("Gote wins!")
        if (pl == 1 && state[41]==1) title("Sente wins!")
        if (pl == 0 && state[45]!=1) title("Gote wins!")
        if (pl == 1 && state[41]!=1) title("Sente wins!")
      }
    }
  }
} 
if (diagnostic_mode) sink()
