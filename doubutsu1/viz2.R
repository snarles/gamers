library(png)

k <- 8
roundedSq <- cbind(
  c(0.1, 0.1, 0.1 * cos(seq(pi, pi/2, -pi/k)) + 0.2, 0.2, 0.8, 0.1 * cos(seq(pi/2, 0, -pi/k)) + 0.8, 0.9, 0.9, 0.1 * cos(seq(0, -pi/2, -pi/k)) + 0.8, 0.8, 0.2, 0.1 * cos(seq(-pi/2, -pi, -pi/k)) + 0.2),
  c(0.2, 0.8, 0.1 * sin(seq(pi, pi/2, -pi/k)) + 0.8 ,0.9, 0.9, 0.1 * sin(seq(pi/2, 0, -pi/k)) + 0.8, 0.8, 0.2, 0.1 * sin(seq(0, -pi/2, -pi/k)) + 0.2, 0.1, 0.1, 0.1 * sin(seq(-pi/2, -pi, -pi/k)) + 0.2)
)

# plot(NA, NA, xlim = c(-0.7, 3.7), ylim = c(-0.7, 4.5), asp = TRUE, ann = FALSE, axes = FALSE)

# board <- readPNG("graphics/board.png")
# rasterImage(board, -0.415, -0.72, 3.42, 4.75)
# #polygon(c(0,0,3,3), c(0,4,4, 0))
# img <- readPNG("graphics/s_k.png")
# polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 0, col = "black", border = "black")
# rasterImage(img, 1.2, 0.2, 1.8, 0.8)
# img <- readPNG("graphics/s_b.png")
# polygon(roundedSq[, 1] + 2, roundedSq[, 2] + 0, col = "black", border = "black")
# rasterImage(img, 2.2, 0.2, 2.8, 0.8)
# img <- readPNG("graphics/s_r.png")
# polygon(roundedSq[, 1] + 0, roundedSq[, 2] + 0, col = "black", border = "black")
# rasterImage(img, 0.2, 0.2, 0.8, 0.8)
# img <- readPNG("graphics/s_p.png")
# polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 1, col = "black", border = "black")
# rasterImage(img, 1.2, 1.2, 1.8, 1.8)
# img <- readPNG("graphics/g_k.png")
# polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 3, col = "white", border = "black")
# rasterImage(img, 1.2, 3.2, 1.8, 3.8)
# img <- readPNG("graphics/g_b.png")
# polygon(roundedSq[, 1] + 0, roundedSq[, 2] + 3, col = "white", border = "black")
# rasterImage(img, 0.2, 3.2, 0.8, 3.8)
# img <- readPNG("graphics/g_r.png")
# polygon(roundedSq[, 1] + 2, roundedSq[, 2] + 3, col = "white", border = "black")
# rasterImage(img, 2.2, 3.2, 2.8, 3.8)
# img <- readPNG("graphics/g_p.png")
# polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 2, col = "white", border = "black")
# rasterImage(img, 1.2, 2.2, 1.8, 2.8)


init_state <- c(0,
                0,
                0,
                0,
                2,1,0,  1,1,0,  3,1,0,
                0,0,0,  4,1,0,  0,0,0, 
                0,0,0,  4,0,0,  0,0,0,
                3,0,0,  1,0,0,  2,0,0,
                0,0,0,0,
                0,0,0,0,
                0,0,0)


draw_board <- function(coords = TRUE) {
  plot(NA, NA, xlim = c(-0.7, 3.7), ylim = c(-0.7, 4.5), asp = TRUE, ann = FALSE, axes = FALSE)
  board <- readPNG("graphics/board.png")
  rasterImage(board, -0.415, -0.72, 3.42, 4.75)
  if (coords) {
    cl <- "orange"
    points(rep(-0.18, 4), 0.53 + 0:3, col = "white", pch = 19, cex = 2)
    points( 0.5 + 0:2, rep(4.15, 3), col = "white", pch = 19, cex = 2)
    text(-0.18, 0.53, "d", col = cl)
    text(-0.18, 1.53, "c", col = cl)
    text(-0.18, 2.53, "b", col = cl)
    text(-0.18, 3.53, "a", col = cl)
    text(0.5, 4.15, "3", col = cl)
    text(1.5, 4.15, "2", col = cl)
    text(2.5, 4.15, "1", col = cl)
  }
}

draw_piece <- function(x, y, ptype, pl, prom) {
  if (pl == 0) {
    polygon(roundedSq[, 1] + x - 1, roundedSq[, 2] + y - 1, col = "black", border = "black")
    pstr <- "s_"
  } else {
    polygon(roundedSq[, 1] + x - 1, roundedSq[, 2] + y - 1, col = "white", border = "black")
    pstr <- "g_"
  }
  pstr <- paste0(pstr, c("k", "r", "b", "p", "t")[ptype + prom])
  img <- readPNG(paste0("graphics/", pstr, ".png"))
  rasterImage(img, x - 1 + .2, y - 1 + .2, x -.2, y -.2)
  polygon(c(0.2, 0.2, 0.8, 0.8) + x -1, c(0.2, 0.8, 0.8, 0.2) + y - 1, border = rgb(0,0,0,1), lwd = 2)
}

sel_piece <- function(x, y) {
  polygon(c(0.2, 0.2, 0.8, 0.8) + x -1, c(0.2, 0.8, 0.8, 0.2) + y - 1, border = rgb(1,1,0,1), lwd = 2)
}
unsel_piece <- function(x, y) {
  polygon(c(0.2, 0.2, 0.8, 0.8) + x -1, c(0.2, 0.8, 0.8, 0.2) + y - 1, border = rgb(0,0,0,1), lwd = 2)
}


PIECESTRS <- cbind(c(" ", "K","R","B","P"), c(" ", "k", "r", "b", "p"))

LOX <- c("3a", "2a", "1a", 
         "3b", "2b", "1b",
         "3c", "2c", "1c",
         "3d", "2d", "1d"
)

movestr <- function(movev) {
  paste(PIECESTRS[movev[1] + 1, 1],
        c("**", LOX)[movev[2] + 1],
        c("**", LOX)[movev[3] + 1],
        sep = "-")
}
draw_state <- function(state, coords = TRUE, title = FALSE) {
  xs <- rep(1:3, 4)
  ys <- rep(4:1, each = 3)
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  movev <- state[49:51]
  draw_board(coords)
  for (i in 1:12) {
    if (board[1, i] != 0) {
      draw_piece(xs[i], ys[i], board[1, i], board[2, i], board[3, i])
    } 
  }
  for (i in 1:3) {
    if (hand1[i+1] > 0) {
      if (hand1[i + 1] > 1) {
        draw_piece(4.5, i, i+1, 0, 0)
      }
      draw_piece(5, i, i+1, 0, 0)
    }
    if (hand2[i+1] > 0) {
      if (hand2[i + 1] > 1) {
        draw_piece(-.5, 5-i,i+1, 1, 0)
      }
      draw_piece(-1, 5-i, i+1, 1, 0)
    }
  }
  
  if (title) {
    if (movev[1] != 0) {
      title(paste0(state[4], ". ", movestr(movev)))
    } else {
      title(paste(c("Sente", "Gote")[state[4] %% 2 + 1], "to move"))
    }
  }
}

