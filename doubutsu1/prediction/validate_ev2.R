source("doubutsu1/lg_analysis_setup.R")
Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")
lmoves <- readRDS("doubutsu1/lmoves.rds")
gstates <- readRDS("doubutsu1/lg_states.rds")
source("doubutsu1/viz.R")

eye11 <- pracma::eye(11)
ut3 <- pracma::eye(3); ut3[upper.tri(ut3)] <- 1
expand_state <- function(state) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  ptypes <- board[1, ] + board[2, ] * 5 + board[3, ]
  ptypes[board[1, ]==0] <- 0
  bboard <- as.numeric(eye11[, ptypes + 1][-1, ])
  names(bboard) <- paste(rep(c("K", "R", "B", "P", "T", "k", "r", "b", "p", "t"), 12), 
                         rep(1:12, each = 10), sep = "")
  h1 <- as.numeric(ut3[, hand1 + 1][-1, ])
  h2 <- as.numeric(ut3[, hand2 + 1][-1, ])
  names(h1) <- paste(rep(c("K", "R", "B", "P"), each = 2), rep(c("x1", "x2")), sep = "")
  names(h2) <- paste(rep(c("K", "R", "B", "P"), each = 2), rep(c("x1", "x2")), sep = "")
  ans <- c(bboard, h1, h2)
  ## length(ans) ## 136
  ans
}

bt <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")

## get some sente win states and sente loss states
filt1 <- rep(FALSE, nrow(gametable)); filt1[teinds] <- TRUE
filt1 <- filt1 & (gametable$len > 7)
swin <- gametable$sente == gametable$winner
slose <- gametable$sente == gametable$loser

set.seed(0)
wininds <- sort(sample(which(filt1 & swin), 3))
loseinds <- sort(sample(which(filt1 & slose), 3))

winstates <- list()
losestates <- list()

for (ind in wininds) {
  ll <- gametable$len[ind]
  state <- gstates[[ind]][[ll - 4]]
  winstates <- c(winstates, list(state))
}
for (ind in loseinds) {
  ll <- gametable$len[ind]
  state <- gstates[[ind]][[ll - 3]]
  losestates <- c(losestates, list(state))
}

states <- c(winstates, losestates)
stateexp <- lapply(states, expand_state)
X <- do.call(rbind, stateexp)
pr <- predictX(X, bt)
pr


for (i in 1:6) {
  pdf(paste0("doubutsu1/presentation/val", i, ".pdf"))
  draw_state(states[[i]])
  title(floor(pr[i] * 100)/100, cex.main = 5)
  dev.off()
}


####
##  Get stats
####

wininds <- which(filt1 & swin)
loseinds <- which(filt1 & slose)

winstates <- list()
losestates <- list()

for (ind in wininds) {
  ll <- gametable$len[ind]
  state <- gstates[[ind]][[ll - 4]]
  winstates <- c(winstates, list(state))
}
for (ind in loseinds) {
  ll <- gametable$len[ind]
  state <- gstates[[ind]][[ll - 3]]
  losestates <- c(losestates, list(state))
}

states <- c(winstates, losestates)
stateexp <- lapply(states, expand_state)
X <- do.call(rbind, stateexp)
pr <- predictX(X, bt)
pr

pr1 <- pr[1:length(winstates)]
pr2 <- pr[-(1:length(winstates))]
boxplot(pr1, pr2, names = c("win", "lose"))

sp <- (mean(pr1) + mean(pr2))/2
abline(sp, 0, col = "red")
accsplit <- .5 * sum(pr1 > sp)/length(pr1) + .5 * sum(pr2 < sp)/length(pr2)
title(floor(accsplit * 100)/100)
