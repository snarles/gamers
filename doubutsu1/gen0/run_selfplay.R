source("doubutsu1/gen0/sourceP.R")
source("doubutsu1/gen0/selfplay.R")
Bs <- readRDS("doubutsu1/prediction/spfit1_sente.rds")
Bg <- readRDS("doubutsu1/prediction/spfit1_gote.rds")
nsample = 3; mateXdepth = 5
games <- list()

while (i < 10000) {
  res <- selfplay(i, Bs, Bg, nsample, mateXdepth)
  games[[i]] <- res
  if (i %% 100 == 0) {
    saveRDS(games, "doubutsu1/gen0/selfplays00.rds")
  }
  i <- i + 1
}

