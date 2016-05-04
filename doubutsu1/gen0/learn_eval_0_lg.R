#####
##  Learn eval function to counter self-plays, plus little golem
#####

source("doubutsu1/gen0/source.R")
source("doubutsu1/prediction/multinom2.R")
sourceCpp("doubutsu1/prediction/interaction2a.cpp")
enames <- readRDS("doubutsu1/i1names.rds")

mcm_probs <- function(mat, bt) {
  ips <- predict2(mat, bt)
  ps <- exp(ips)
  ps/sum(ps)
}

expgame <- function(games) {
  egames <- t(apply(games, 1, expandState))
  colnames(egames) <- enames
  egames
}

train <- function(X, Y, bt = NULL, eps, nits) {
  if (is.null(bt))   bt <- numeric(ncolsX(136))
  for (i in 1:nits) {
    ps <- predictX(X, bt)
    ps <- exp(ps)/(1 + exp(ps))
    resid <- Y - ps
    bt <- gradientX(bt, X, eps * resid)
    mean(ps[Y==1])
    if (i %% 5 == 0) {
      print(c(loglik = sum(Y * log(ps) + (1-Y) * log(1-ps)), mean(ps[Y==1]), mean(ps[Y==0])))
    }
  }
  return(bt)
}

trainALT <- function(bt, alts, sente = FALSE, eps = eps) {
  pl <- 0
  pss <- numeric()
  if (sente) pl <- 1
  for (i in 1:length(alts)) {
    for (j in 1:length(alts[[i]])) {
      if (j %% 2==pl) {
        alt <- alts[[i]][[j]]
        y <- which(alt[, 1]==1)
        X <- t(apply(alt, 1, expandState))
        ps <- mcm_probs(X, bt)
        pss <- c(pss, ps[y])
        ps[y] <- ps[y] - 1
        bt <- gradientX(bt, X, -eps * ps)
      }
    }
  }
  print(sum(log(pss))/length(pss))
  bt
}


####
##  EXE CODE
####

games <- readRDS("doubutsu1/gen0/selfplaysE00.rds")
games <- lapply(games, function(v) {
  mat <- do.call(rbind, v$slist)
  if (length(v$mlist) %% 2 == 0) {
    # sente win
    mat[mat[, 4] %% 2 == 0, 1] <- 0
    mat[mat[, 4] %% 2 == 1, 1] <- 1
  } else {
    mat[mat[, 4] %% 2 == 0, 1] <- 1
    mat[mat[, 4] %% 2 == 1, 1] <- 0
  }
  mat
})
games <- do.call(rbind, games)
gamesS <- games[games[, 4] %% 2 == 1, ]
gamesG <- games[games[, 4] %% 2 == 0, ]
egamesS <- expgame(gamesS)
egamesG <- expgame(gamesG)


load('doubutsu1/lg_data.rda')
ginds <- which(gametable$len > 2)
# glist <- glist[ginds]
# lmvs <- readRDS("doubutsu1/lmoves.rds")[ginds]
alts <- readRDS("doubutsu1/altMoves.rds")[ginds]

btS <- train(egamesS, gamesS[, 1], eps = 1e-4, nits = 20)
btS <- train(egamesS, gamesS[, 1], btS, eps = 1e-4, nits = 500)
# saveRDS(btS, "doubutsu1/gen0/temp_evS.rds")
# saveRDS(btS, "doubutsu1/gen0/n01_evS.rds")

eps <- 1e-4
btG <- train(egamesG, gamesG[, 1], NULL, eps, nits = 20)
for (i in 1:100) {
  btG <- train(egamesG, gamesG[, 1], btG, eps, nits = 20)
  btG <- trainALT(btG, alts, FALSE, eps)
}



# saveRDS(btG, "doubutsu1/gen0/temp_evG.rds")
# saveRDS(btG, "doubutsu1/gen0/lg01_evG.rds")
