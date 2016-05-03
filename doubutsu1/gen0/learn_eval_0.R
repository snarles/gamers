#####
##  Learn eval function to counter self-plays
#####

source("doubutsu1/gen0/source.R")
sourceCpp("doubutsu1/prediction/interaction2a.cpp")

games <- readRDS("doubutsu1/gen0/selfplays00.rds")
games <- lapply(games, function(v) {
  mat <- do.call(rbind, v$slist)
  if (length(v$mlist) %% 2 == 0) {
    # sente win
    mat[mat[, 4] %% 2 == 0, 1] <- 1
    mat[mat[, 4] %% 2 == 1, 1] <- 0
  } else {
    mat[mat[, 4] %% 2 == 0, 1] <- 0
    mat[mat[, 4] %% 2 == 1, 1] <- 1
  }
  mat
})
games <- do.call(rbind, games)


