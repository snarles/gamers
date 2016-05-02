library(png)

plot(NA, NA, xlim = c(0, 4), ylim = c(0, 5), asp = TRUE)
polygon(c(1, 2, 2, 1), c(1, 1, 2, 2))


img <- readPNG("graphics/s_k.png")
rasterImage(img, 1, 1, 2, 2)
