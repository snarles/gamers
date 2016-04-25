####
##  Adjacency game
##  Place tokens in empty cells [1...N]
##  Lose if place adjacent to occupied cell
####

library(Rcpp)
sourceCpp("benchmarks/adjacent2.cpp")
moveValues(c(0, 1, 0), 1)
moveValues(c(1, 0, 0, 0), 1)
