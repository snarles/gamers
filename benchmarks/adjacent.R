####
##  Adjacency game
##  Place tokens in empty cells [1...N]
##  Lose if place adjacent to occupied cell
####

library(Rcpp)
sourceCpp("benchmarks/adjacent1.cpp")
t1 <- proc.time()
moveValues(rep(0, 18), 10)
proc.time() - t1

sourceCpp("benchmarks/adjacent2.cpp")
t1 <- proc.time()
moveValues(rep(0, 18), 10)
proc.time() - t1
