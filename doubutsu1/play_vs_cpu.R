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



query_move <- function(state) {
  xs <- rep(1:3, 4)
  ys <- rep(4:1, each = 3)
  pl <- state[4] %% 2
  if (state[4] > 0) {
    last_mv <- movestr(state[49:51])
    opponent <- c("Gote", "Sente")[state[4] %% 2 + 1]
    catn(paste(opponent, "moved", last_mv))
  }
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  hand1st <- c(); hand2st <- c()
  for (i in 1:4) {
    hand1st <- c(hand1st, rep(PIECESTRS[i + 1, 1], hand1[i]))
    hand2st <- c(hand2st, rep(PIECESTRS[i + 1, 1], hand2[i]))
  }
  mvs <- legal_moves(state)
  draw_state(state, title = TRUE)
  title(sub= "QUIT")
  # print(mvs)
  flag <- TRUE
  sel <- c(NA, NA)
  while(flag) {
    guess <- query_location()
    if (is.na(guess)[1]) {
      
    } else if (guess[1]==-Inf) {
      print("QUIT")
      flag <- FALSE
    } else {
      print("SELECTED")
      xy <- guess
      if ((xy[1] <= 3) && (xy[1] >= 1) && (xy[2] <= 4) && (xy[2] >= 1)) {
        start <- xy_to_start[xy[2], xy[1]]
        if (board[2, start]==pl) {
          if (!is.na(sel[1])) {
            unsel_piece(sel[1], sel[2])
          }
          sel <- xy
          sel_piece(xy[1], xy[2])
        }
      }
      if ((xy[1] == -1) && (pl ==1) && (xy[2] <= 4) && (xy[2] >= 5-sum(hand2))) {
        if (!is.na(sel[1])) {
          unsel_piece(sel[1], sel[2])
        }
        sel <- xy
        sel_piece(xy[1], xy[2])
      }
      if ((xy[1]== 5) && (pl == 0) && (xy[2] >= 1) && (xy[2] <= sum(hand1))) {
        if (!is.na(sel[1])) {
          unsel_piece(sel[1], sel[2])
        }
        sel <- xy
        sel_piece(xy[1], xy[2])
      }
    }
  }
  
  
  
}

query_location <- function() {
  pts <- locator(1); pts <- c(x=pts$x, y=pts$y)
  if (abs(pts[1] - 1.5) < 1 && pts[2] < 0) {
    return(c(-Inf, -Inf))
  }
  guess <- round(pts + 0.5)
  if (sum(abs((pts + 0.5) - guess)) < 0.7) {
    return(guess)
  }
  c(NA, NA)
}

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

