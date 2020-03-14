## tries to find balanced metagames with speed-attack-def mechanics

evaluate_winner <- function(v1, v2) {
  if (v1[1] > v2[1]) {
    speed_mult <- c(2, 1)
  }
  else if (v1[1]==v2[1]) {
    speed_mult <- c(1, 1)
  }
  else {
    speed_mult <- c(1, 2)
  }
  a_minus_d <- c(v1[2] - v2[3], v2[2] - v1[3])
  a_minus_d[a_minus_d < 0] <- 0
  final_val <- speed_mult * a_minus_d
  if (final_val[1] > final_val[2]) {
    return(1)
  }
  else if (final_val[1]==final_val[2]) {
    return(0)
  }
  else {
    return(-1)
  }
}

evaluate_winner(c(5, 4, 1), c(6, 3, 1))

matchup_matrix <- function(vs) {
  n <- dim(vs)[1]
  mat <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      mat[i, j] <- evaluate_winner(vs[i, ], vs[j, ])
    }
  }
  return(mat)
}

vs_test <- rbind(c(6, 220, 60), c(6, 380, -120), c(5, 360, 60), c(4, 540, 60), c(4, 750, -120))
matchup_matrix(vs_test)

n <- 50
vs_random <- matrix(0, n, 3)
colnames(vs_random) <- c("spd", "atk", "def")
#vs_random[, 1] <- sample(5, n, TRUE, c(0.1, 0.3, 0.3, 0.2, 0.1))
vs_random[, 1] <- sample(5, n, TRUE)
vs_random[, 3] <- sample(25, n, TRUE)
vs_random[, 2] <- sample(50, n, TRUE) + 25
mm <- matchup_matrix(vs_random)
rs <- rowSums(mm)

for (i in 1:500) {
  vs_random[order(rs)[1], 2] <- vs_random[order(rs)[1], 2]+1
  vs_random[order(-rs)[1], 2] <- vs_random[order(-rs)[1], 2]-1
  mm <- matchup_matrix(vs_random)
  rs <- rowSums(mm)
  #cbind(vs_random, rs)[order(-rs), ]
}
c(min(rs), max(rs))/n
cbind(vs_random, rs)[order(-rs), ]
plot(vs_random[,1], jitter(rs))
plot(vs_random[,3], jitter(rs))


library(ggplot2)
vs <- data.frame(vs_random)
ggplot(vs, aes(atk, def, colour = spd)) + 
  geom_point()


vs_filt <- floor(vs_random)
mm <- matchup_matrix(vs_filt)
rs <- rowSums(mm)
c(min(rs), max(rs))/n

rs <- rowSums(mm)
inds_keep <- (rs != min(rs)) & (rs != max(rs))
#inds_keep <- (rs != max(rs))
vs_filt <- vs_filt[inds_keep, ]
mm <- mm[inds_keep, inds_keep]
rs <- rowSums(mm)
c(min(rs), max(rs))/n

vs_filt <- unique(vs_filt)

mm <- matchup_matrix(vs_filt)
rs <- rowSums(mm)
cbind(vs_filt, rs)[order(-rs), ]


v <- rep(1/dim(mm)[1], dim(mm)[1])

for (i in 1:2000) {
  v <- v * (1+ (mm %*% v)/100)
}
max(mm %*% v)
sum(v)
t(floor(v* 100))

cbind(vs_filt, floor(v * 100))[order(-v), ]

vs_filt <- vs_filt[v > 0.01, ]
mm <- matchup_matrix(vs_filt)
v <- rep(1/dim(mm)[1], dim(mm)[1])

for (i in 1:2000) {
  v <- v * (1+ (mm %*% v)/100)
}
max(mm %*% v)
sum(v)
t(floor(v* 100))

cbind(vs_filt, floor(v * 100))[order(-v), ]

vs <- data.frame(vs_filt)
vs[, 1] <- as.factor(vs[, 1])
ggplot(vs, aes(atk, def, colour = spd)) + 
  geom_point()

cbind(vs, 1.3 * vs[, 2] + vs[, 3])[vs[, 1]==4, ]
