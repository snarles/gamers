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


mateX <- function(state, maxdepth) {
  flag <- TRUE
  depth <- 0
  while (flag) {
    val <- maxVal(state, state[4] + depth)
    if (val != 0) {
      return(depth * val)
    }
    depth <- depth  + 1
  }
  return(NA)
}

####
##  New code mate in X
####
mlist <- list()
t1 <- proc.time()
maxdepth <- 5
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
hs <- names(mlist)[deepness == 3]
h <- sample(hs, 1)
state <- hashtab[[h]]
print_state(state)

mateX(state)

t1 <- proc.time()
maxVal(state, state[4] + 8)
proc.time() - t1

# st2 <- move(state, 8, 6, 0)
# print_state(st2)
# maxVal(st2, st2[4] + 1)
# 
# st3 <- move(state, 12, 9, 0)
# print_state(st3)
# maxVal(st3, st3[4] + 0)
