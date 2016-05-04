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

query_move <- function(state) {
  if (state[4] > 0) {
    last_mv <- movestr(state[49:51])
    opponent <- c("Gote", "Sente")[state[4] %% 2 + 1]
    catn(paste(opponent, "moved", last_mv))
  }
  mvs <- legal_moves(state)
  # print(mvs)
  flag <- TRUE
  while(flag) {
    x <- readline("What will you move? [X = quit, P = print] ")
    if (x == "X") {
      return("quit")
    }
    if (x == "P") {
      print_state(state, TRUE)
      print(sort(mvs))
    }
    if (x == "V") {
      draw_state(state)
      title("Cheater~~!", sub = paste(sort(mvs), collapse = ", "))
      print(sort(mvs))
    }
    if (x %in% mvs) return(x)
  }
}

catn("Play as sente (0) or gote (1)?:")
x <- readline("Play as sente (0) or gote (1)?:")
if (x=="0") pl <- "sente"
if (x=="1") pl <- "gote"
game_record <- character()
game_states <- list()

if (x %in% c("0", "1")) {
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
      mX <- mateX(state, 3)
      game_states <- c(game_states, list(state))
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
} else {
  print("invalid.. choose 0 or 1 next time")
}

