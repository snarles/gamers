

source("doubutsu1/prediction/multinom2.R")

Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")


tab <- readRDS("doubutsu1/prediction/player_ranks.rds")
players <- rownames(tab)
(player <- players[order(-tab$total)[5]])

(select <- players[tab$rating > 0.2 & tab$total > 5])
#(select <- players[tab$rating < -0.2 & tab$total > 5])

mats <- resTr$senteAlts
choice <- resTr$senteChoice
pl <- resTr$sentePl

## baseline

wtrange <- seq(0, 10, 0.25)
accs <- 0 * wtrange
filt <- which(resTe$sentePl %in% select)

for (i in 1:length(wtrange)) {
  bt <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
  weights <- 1 + wtrange[i] * (pl %in% select)
  weights <- weights/sum(weights) * length(weights)
  bt <- mcm_sgd(mats, choice, bt, l1p = 2e-5, l2p = 1e-5, eps = 0.01, weights = weights)
  res <- mcm_loss(resTe$senteAlts[filt], resTe$senteChoice[filt], resTe$senteMoves[filt], bt)
  accs[i] <- res[[1]]
}
plot(wtrange, accs, type = "l")
title("Top players")
# title("Least-rated players")

# bt <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
# weights <- 1 + 2 * (pl %in% select)
# weights <- weights/sum(weights) * length(weights)
# bt <- mcm_sgd(mats, choice, bt, l1p = 2e-5, l2p = 1e-5, eps = 0.01, weights = weights)
# 
# saveRDS(bt, "doubutsu1/prediction/multinom_fit2_sente_topPL.rds")



p <- ncol(mats[[1]]) # base dimension
nms1 <- colnames(mats[[1]])
nms2 <- c()
for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    nms2 <- c(nms2, paste(nms1[i], nms1[j], sep = "."))
  }
}
nms <- c(nms1, nms2)
bt0 <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bt1 <- readRDS("doubutsu1/prediction/multinom_fit2_sente_topPL.rds")
names(bt0) <- nms
names(bt1) <- nms

plot(bt0, bt1)
sort(bt0 - bt1, decreasing = TRUE)[1:10]
sort(bt0 - bt1)[1:10]
