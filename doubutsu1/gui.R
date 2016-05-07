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
  board[2, board[1, ]==0] <- -1
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
  ptype <- NA
  handsel <- FALSE
  while(flag) {
    guess <- query_location()
    if (is.na(guess)[1]) {
      print("Trying clicking in the middle of a square.")
    } else if (guess[1]==-Inf) {
      print("QUIT")
      flag <- FALSE
    } else {
      xy <- guess
      # print(paste("SELECTED", LOX[xy_to_start[xy[2], xy[1]]]))
      if ((xy[1] <= 3) && (xy[1] >= 1) && (xy[2] <= 4) && (xy[2] >= 1)) {
        start <- xy_to_start[xy[2], xy[1]]
        if (board[2, start]==pl) {
          if (!is.na(sel[1])) {
            unsel_piece(sel[1], sel[2])
          }
          ptype <- board[1, start]
          loc <- LOX[xy_to_start[xy[2], xy[1]]]
          sel <- xy
          handsel <- FALSE
          sel_piece(xy[1], xy[2])
          print(paste("Selected", PTYPES[ptype], "at", loc))
        } else if (!is.na(sel[1])) {
          if (handsel) {
            loc1 <- "**"
          } else {
            loc1 <- LOX[xy_to_start[sel[2], sel[1]]]
          }
          loc2 <- LOX[xy_to_start[xy[2], xy[1]]]
          mv <- paste(PTYPES[ptype], loc1, loc2, sep = "-")
          if (mv %in% mvs) {
            return(mv)
          } else {
            print("Illegal move!")
          }
        } else {
          print("Select one of your pieces first.")
        }
      }
      if ((xy[1] == -1) && (pl ==1) && (hand2[6 - xy[2]] > 0)) {
        if (!is.na(sel[1])) {
          unsel_piece(sel[1], sel[2])
        }
        ptype <- 6 - xy[2]
        sel <- xy
        sel_piece(xy[1], xy[2])
        handsel <- TRUE
        print(paste("Selected", PTYPES[ptype], "in hand"))
      }
      if ((xy[1]== 5) && (pl == 0) && (hand1[xy[2] + 1] > 0)) {
        ptype <- xy[2] + 1
        if (!is.na(sel[1])) {
          unsel_piece(sel[1], sel[2])
        }
        sel <- xy
        sel_piece(xy[1], xy[2])
        handsel <- TRUE
        print(paste("Selected", PTYPES[ptype], "in hand"))
      }

    }
  }
  
  return("resign")
  
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