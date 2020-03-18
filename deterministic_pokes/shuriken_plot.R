
circle <- cbind(sin(1:24/24 * 2 * pi), cos(1:24/24 * 2 * pi))
deca <- cbind(sin(0:12/12 * 2 * pi),cos(0:12/12 * 2 * pi))
plot(NA, NA, xlim = c(-2, 2), ylim = c(-2, 2), axes = FALSE, asp=1)
#polygon(circle, col='black')
#lines(deca)
mult <- 3
modifier <- rep(c(mult, 1), 7)[1:13]
shuriken <- .6 * modifier * deca
lines(shuriken, lwd=3)
shuriken <- shuriken[c(12, 1:12), ]
#shuriken <- shuriken[c(2:13, 1), ]
#shuriken[, 1] <- shuriken[, 1]
# for (i in 1:13) text(shuriken[i, 1], shuriken[i, 2], i)



shuriken2 <- matrix(NA, 0, 2)
interp <- 0:3/4
shuriken
for (i in 1:12) {
  shuriken2 <- rbind(shuriken2, t(t(interp)) %*% shuriken[i+1, , drop = FALSE] + t(t(1-interp)) %*% shuriken[i, ,drop=FALSE])
}
shuriken2 <- rbind(shuriken2, shuriken[1, ])
points(shuriken2)

colortable <- c( 0,
  1, 1, 2, 3, 5, 1, 6, 0, 
  5, 5, 3, 1, 4, 5, 2, 0, 
  4, 4, 1, 5, 6, 4, 3, 0, 
  6, 6, 5, 4, 2, 6, 1, 0, 
  2, 2, 4, 6, 3, 2, 5, 0, 
  3, 3, 6, 2, 1, 3, 4
) + 1

colors <- c("black","yellow","coral",
            "gray94", "cyan3", 
            "maroon4", "lawngreen")
tcolors <- c("white","black","black",
            "black", "black", 
            "white", "black")

theta <- 0.7
scaling <- 0.2
rotmat1 <- rbind(t(c(cos(theta), sin(theta))),
                 t(c(-sin(theta), cos(theta))))


for (i in 1:48) {
  polygon(shuriken2[i, 1] + 0.06 * circle[, 1],
          shuriken2[i, 2] + 0.06 * circle[, 2],
          col='black')
  polygon(shuriken2[i, 1] + 0.05 * circle[, 1],
          shuriken2[i, 2] + 0.05 * circle[, 2],
          col=colors[colortable[i]])
  diff <- shuriken2[i+1,] - shuriken2[i,]
  endpt <- shuriken2[i, ] + 0.77 * diff
  arrowtip <- scaling * rotmat1 %*% diff
  lines(endpt[1]+c(0, -arrowtip[1]),
        endpt[2]+c(0, -arrowtip[2]),
        lwd=3)
  arrowtip <- scaling * t(rotmat1) %*% diff
  lines(endpt[1]+c(0, -arrowtip[1]),
        endpt[2]+c(0, -arrowtip[2]),
        lwd=3)
  text(shuriken2[i, 1]-0.005, shuriken2[i, 2]+0.003, colortable[i]-1, 
       col = tcolors[colortable[i]], cex = 0.8)
  text(shuriken2[i, 1]+0.005, shuriken2[i, 2]+0.003, colortable[i]-1, 
       col = tcolors[colortable[i]], cex = 0.8)
}



#lines(shuriken %*% rotmat1)
#lines(shuriken %*% t(rotmat1))
