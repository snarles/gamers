#####
##  Learn eval function to counter self-plays
#####

source("doubutsu1/gen0/source.R")
sourceCpp("doubutsu1/prediction/interaction2a.cpp")
enames <- readRDS("doubutsu1/i1names.rds")


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
    print(c(loglik = sum(Y * log(ps) + (1-Y) * log(1-ps)), mean(ps[Y==1]), mean(ps[Y==0])))
  }
  return(bt)
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

btS <- train(egamesS, gamesS[, 1], eps = 1e-4, nits = 20)
btS <- train(egamesS, gamesS[, 1], btS, eps = 1e-4, nits = 500)
# saveRDS(btS, "doubutsu1/gen0/temp_evS.rds")
# saveRDS(btS, "doubutsu1/gen0/n01_evS.rds")

btG <- train(egamesG, gamesG[, 1], eps = 1e-4, nits = 20)
btG <- train(egamesG, gamesG[, 1], btG, eps = 1e-4, nits = 500)
# saveRDS(btG, "doubutsu1/gen0/temp_evG.rds")
# saveRDS(btS, "doubutsu1/gen0/n01_evG.rds")
