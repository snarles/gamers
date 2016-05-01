
load("doubutsu1/lg_data.rda", verbose = TRUE)
players <- sort(unique(c(gametable$sente, gametable$gote)))

filt <- (gametable$len > 2) & (gametable$winner != "")

n <- sum(filt)
gt <- gametable[filt, ]
p <- length(players)

X <- matrix(0, n, p)
colnames(X) <- players
y <- (gt$sente == gt$winner) + 0
for (i in 1:n) {
  X[i, gt$sente[i]] <- 1
  X[i, gt$gote[i]] <- -1
}

library(glmnet)
res <- glmnet(X, y, alpha = 0)
cf0 <- coef(res, s = 0)[-1]
names(cf0) <- players
wins <- sapply(players, function(v) sum(gt$winner == v))
losses <- sapply(players, function(v) sum(gt$loser == v))
tab <- data.frame(rating = cf0, wins, losses, total = wins + losses)
View(tab[order(-cf0), ])

## bootstrap coefficients
mc.reps <- 100
cfs <- matrix(0, mc.reps, p)
for (i in 1:mc.reps) {
  inds <- sample(n, n, TRUE)
  res <- glmnet(X[inds, ], y[inds], alpha = 0)
  cfs[i, ] <- coef(res, s = 0)[-1]
}
xl <- c(-1.5, 1.0)
boxplot(cfs[, 1:23], horizontal = TRUE, names = paste(1:23), ylim = xl)
boxplot(cfs[, 24:46], horizontal = TRUE, names = paste(24:46), ylim = xl)
boxplot(cfs[, 47:69], horizontal = TRUE, names = paste(47:69), ylim = xl)

boxplot(cfs, horizontal = TRUE, ylim = xl)
points(cf0, 1:p, pch = "+", col = "red")

o <- order(cf0)
boxplot(cfs[, o], horizontal = TRUE, ylim = xl)
points(cf0[o], (1:p), pch = "+", col = "red")

sort(cf0, decreasing = TRUE)


## filter by total
filt <- tab$total > 10
o <- order(cf0[filt])
boxplot(cfs[, filt][, o], horizontal = TRUE, ylim = xl)
points(cf0[filt][o], 1:sum(filt), pch = "+", col = "red")
nms <- players[filt][o]
cf2 <- cfs[, filt][, o]
for (i in 1:sum(o)) {
  text(min(cf2[, i]) - 0.5, i, nms[i])
}

saveRDS(tab, "doubutsu1/prediction/player_ranks.rds")
