

source("doubutsu1/prediction/multinom2.R")

Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")


tab <- readRDS("doubutsu1/prediction/player_ranks.rds")
players <- rownames(tab)
(player <- players[order(-tab$total)[3]])

mats <- resTr$senteAlts
choice <- resTr$senteChoice
pl <- resTr$sentePl

## baseline

wtrange <- seq(0, 10, 0.25)
accs <- 0 * wtrange
filt <- which(resTe$sentePl == player)

for (i in 1:length(wtrange)) {
  bt <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
  weights <- 1 + wtrange[i] * (pl == player)
  weights <- weights/sum(weights) * length(weights)
  bt <- mcm_sgd(mats, choice, bt, l1p = 2e-5, l2p = 1e-5, eps = 0.01, weights = weights)
  res <- mcm_loss(resTe$senteAlts[filt], resTe$senteChoice[filt], resTe$senteMoves[filt], bt)
  accs[i] <- res[[1]]
}
plot(wtrange, accs, type = "l")
title(player)
