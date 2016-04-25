####
##  Adjacency game
##  Place tokens in empty cells [1...N]
##  Lose if place adjacent to occupied cell
####

library(Rcpp)
sourceCpp("benchmarks/adjacent2.cpp")
