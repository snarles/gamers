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







ii <- 1340
ii <- ii+1
state <- database[ii, ]
# draw_state(state, title = TRUE)
query_move(state)




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

if (pl %in% c("gote", "sente")) {
  flag <- TRUE
  state <- init_state
  game_states <- c(game_states, list(state))
  if (pl == "gote") {
    mv <- opening_move_from_book()
    game_record <- c(game_record, mv)
    state <- move_parser(state, mv)
    game_states <- c(game_states, list(state))
  }
  print_state(state, TRUE)
  while(flag) {

    if (pl == "sente" && state[4]%%2 == 0) {
      mv <- query_move(state)
    }
    if (pl == "gote" && state[4] %% 2 == 1) {
      mv <- query_move(state)
    }
    if (mv == "quit") {
      flag <- FALSE
    }
    game_record <- c(game_record, mv)
    if (mv == "resign") flag <- FALSE
    if (!mv %in% c("resign", "quit")) {
      state <- move_parser(state, mv)
      game_states <- c(game_states, list(state))
      mX0 <- mateX(state, 0)
      if (!is.na(mX0) && mX0 == 0) {
        catn("===YOU WIN!!===")
        draw_state(state)
        title("Victory!", sub = pl)
        flag <- FALSE
      }
      if (flag) {
        print("AI THINKING")
        sink("temp.txt")
        mv <- next_move(state)
        sink()
        game_record <- c(game_record, mv)
        state <- move_parser(state, mv)
        game_states <- c(game_states, list(state))
        if (mv == "resign") {
          catn("===YOU WIN!!===")
          draw_state(state)
          title("Victory!", sub = pl)
          flag <- FALSE
        }
      }
    }
    if (flag) {
      mX0 <- mateX(state, 0)
      if (!is.na(mX0) && mX0 == 0) {
        last_mv <- movestr(state[49:51])
        opponent <- c("Gote", "Sente")[state[4] %% 2 + 1]
        catn(paste(opponent, "moved", last_mv))
        catn("===YOU LOSE!!===")
        draw_state(state)
        title("Defeat!", sub = pl)
        flag <- FALSE
      }
    }
  }  
} else {
  print("invalid.. choose 0 or 1 next time")
}

