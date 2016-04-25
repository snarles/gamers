####
##  Adjacency game
##  Place tokens in empty cells [1...N]
##  Lose if place adjacent to occupied cell
####

library(Rcpp)
sourceCpp("benchmarks/adjacent1.cpp")
t1 <- proc.time()
moveValues(rep(0, 20), 10)
proc.time() - t1

sourceCpp("benchmarks/adjacent2.cpp")
t1 <- proc.time()
moveValues(rep(0, 20), 10)
proc.time() - t1

t1 <- proc.time()
moveValues(rep(0, 22), 22)
proc.time() - t1

t1 <- proc.time()
moveValues(rep(0, 23), 23)
proc.time() - t1

t1 <- proc.time()
moveValues(rep(0, 24), 24)
proc.time() - t1

t1 <- proc.time()
moveValues(rep(0, 25), 25)
proc.time() - t1
