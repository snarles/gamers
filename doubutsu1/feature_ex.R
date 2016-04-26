####
##  Extract some more features
####

source("doubutsu1/source.R")
sourceCpp("doubutsu1/Rsource.cpp")
games <- readRDS("doubutsu1/lg_states.rds")

itree <- build_tree(init_state, 1, 100)[, -1]

games2 <- lapply(games, function(v) {
  if (is.null(v)) {
    return(NULL)
  }
  ff <- function(x) build_tree(x, depth = 1, nodemax = 100)[-1, , drop = FALSE]
  c(list(itree), lapply(v, ff))
})

saveRDS(games2, "doubutsu1/altMoves.rds")
