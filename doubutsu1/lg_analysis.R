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

sourceCpp("doubutsu1/Rsource2.cpp")

####
##  New code mate in X
####
mlist <- list()
t1 <- proc.time()
maxdepth <- 2
for (depth in 1:maxdepth) {
  for (h in names(hashtab)) {
    state <- hashtab[[h]]
    if (is.null(mlist[[h]])) mlist[[h]] <- numeric(maxdepth)
    mlist[[h]][depth] <- maxVal(state, state[4] + depth)
  }
}
proc.time() - t1

deepness <- sapply(mlist, function(v) sum(v==0))
table(deepness)
hs <- names(mlist)[deepness == 1]
h <- sample(hs, 1)
state <- hashtab[[h]]
print_state(state)
maxVal(state, state[4] + 0)
maxVal(state, state[4] + 1)
maxVal(state, state[4] + 2)
maxVal(state, state[4] + 3)