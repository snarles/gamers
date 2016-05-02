library(png)

k <- 8
roundedSq <- cbind(
  c(0.1, 0.1, 0.1 * cos(seq(pi, pi/2, -pi/k)) + 0.2, 0.2, 0.8, 0.1 * cos(seq(pi/2, 0, -pi/k)) + 0.8, 0.9, 0.9, 0.1 * cos(seq(0, -pi/2, -pi/k)) + 0.8, 0.8, 0.2, 0.1 * cos(seq(-pi/2, -pi, -pi/k)) + 0.2),
  c(0.2, 0.8, 0.1 * sin(seq(pi, pi/2, -pi/k)) + 0.8 ,0.9, 0.9, 0.1 * sin(seq(pi/2, 0, -pi/k)) + 0.8, 0.8, 0.2, 0.1 * sin(seq(0, -pi/2, -pi/k)) + 0.2, 0.1, 0.1, 0.1 * sin(seq(-pi/2, -pi, -pi/k)) + 0.2)
)

plot(NA, NA, xlim = c(-0.7, 3.7), ylim = c(-0.7, 4.5), asp = TRUE, ann = FALSE, axes = FALSE)

board <- readPNG("graphics/board.png")
rasterImage(board, -0.415, -0.72, 3.42, 4.75)
#polygon(c(0,0,3,3), c(0,4,4, 0))

img <- readPNG("graphics/s_k.png")
polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 0, col = "black", border = "black")
rasterImage(img, 1.2, 0.2, 1.8, 0.8)

img <- readPNG("graphics/s_b.png")
polygon(roundedSq[, 1] + 2, roundedSq[, 2] + 0, col = "black", border = "black")
rasterImage(img, 2.2, 0.2, 2.8, 0.8)

img <- readPNG("graphics/s_r.png")
polygon(roundedSq[, 1] + 0, roundedSq[, 2] + 0, col = "black", border = "black")
rasterImage(img, 0.2, 0.2, 0.8, 0.8)

img <- readPNG("graphics/s_p.png")
polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 1, col = "black", border = "black")
rasterImage(img, 1.2, 1.2, 1.8, 1.8)

img <- readPNG("graphics/g_k.png")
polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 3, col = "white", border = "black")
rasterImage(img, 1.2, 3.2, 1.8, 3.8)

img <- readPNG("graphics/g_b.png")
polygon(roundedSq[, 1] + 0, roundedSq[, 2] + 3, col = "white", border = "black")
rasterImage(img, 0.2, 3.2, 0.8, 3.8)

img <- readPNG("graphics/g_r.png")
polygon(roundedSq[, 1] + 2, roundedSq[, 2] + 3, col = "white", border = "black")
rasterImage(img, 2.2, 3.2, 2.8, 3.8)

img <- readPNG("graphics/g_p.png")
polygon(roundedSq[, 1] + 1, roundedSq[, 2] + 2, col = "white", border = "black")
rasterImage(img, 1.2, 2.2, 1.8, 2.8)
