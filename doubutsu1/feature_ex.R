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
