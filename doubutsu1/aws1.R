library(Rcpp)
sourceCpp("doubutsu1/Rsource2.cpp")
source("doubutsu1/source.R")

games <- readRDS("doubutsu1/lg_states.rds")
hashState(games[[20]][[3]])
hash_state(games[[20]][[3]])
hashes <- lapply(games, function(v) sapply(v, hash_state))

hashtab <- list()
for (game in games) {
  for (state in game) {
    hashtab[[hash_state(state)]] = state;
  }
}

mateX <- function(h, maxdepth) {
  state <- hashtab[[h]]
  flag <- TRUE
  depth <- 0
  while (flag) {
    val <- maxVal(state, state[4] + depth)
    if (val != 0) {
      return(depth * val)
    }
    depth <- depth  + 1
    if (depth > maxdepth) flag <- FALSE
  }
  return(NA)
}

####
##  New code mate in X
####
set.seed(0)
hashorder <- sample(names(hashtab), length(hashtab), FALSE)
library(parallel)
t1 <- proc.time()
res <- mclapply(hashorder, mateX, maxdepth = 3, mc.preschedule = FALSE,
                mc.cores = 40)
proc.time() - t1

names(res) <- hashorder
saveRDS(res, "doubutsu1/aws_output.rds")

