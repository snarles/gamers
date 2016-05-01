####
##  Extract some more features
####

source("doubutsu1/source.R")
sourceCpp("doubutsu1/Rsource.cpp")
games <- readRDS("doubutsu1/lg_states.rds")

itree <- build_tree(init_state, 1, 100)[-1, ]

games2 <- lapply(games, function(v) {
  if (is.null(v)) {
    return(NULL)
  }
  ff <- function(x) build_tree(x, depth = 1, nodemax = 100)[-1, , drop = FALSE]
  c(list(itree), lapply(v, ff))
})

for (i in 1:length(games)) {
  if (length(games[[i]]) > 0) {
    for (j in 1:length(games[[i]])) {
      chosen <- games[[i]][[j]]
      alt <- games2[[i]][[j]]
      ind <- prodlim::row.match(chosen[-(1:2)], alt[, -(1:2)])
      if (is.na(ind)) {
        print(c(i, j))
      }
      alt[ind, 1] <- 1
      games2[[i]][[j]] <- alt
    }
    games2[[i]][[j+1]] <- NULL
  }
}


saveRDS(games2, "doubutsu1/altMoves.rds")


lmoves <- games2

movestr <- function(movev) {
  paste(PIECESTRS[movev[1] + 1, 1],
        c("**", LOX)[movev[2] + 1],
        c("**", LOX)[movev[3] + 1],
        sep = "-")
}

for (i in 1:length(games)) {
  if (length(games[[i]]) > 0) {
    for (j in 1:length(games[[i]])) {
      alt <- games2[[i]][[j]]
      mvm <- alt[, 49:51, drop = FALSE]
      lmoves[[i]][[j]] <- apply(mvm, 1, movestr)
    }
  }
  else {
    lmoves[[i]] <- list("resign")
  }
}

saveRDS(lmoves, "doubutsu1/lmoves.rds")


####
##  all non-resign moves
####

lmoves <- readRDS("doubutsu1/lmoves.rds")
temp <-unique(unlist(sapply(lmoves, unlist)))
allmoves <- setdiff(sort(temp), "resign")
allmoves <- stringr::str_sort(allmoves)
(nmoves <- length(allmoves))
saveRDS(allmoves, file = "doubutsu1/allmoves.rds")
