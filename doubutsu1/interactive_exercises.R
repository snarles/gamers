source("doubutsu1/source.R")
Rcpp::sourceCpp("doubutsu1/Rsource.cpp")
hashtab <- readRDS("doubutsu1/hashtab.rds")
#matein <- unlist(readRDS("doubutsu1/aws_output.rds"))
matein <- readRDS("doubutsu1/matein_solver.rds")
# saveRDS(matein, "doubutsu1/matein.rds")
source("doubutsu1/sourceJP.R")
source("doubutsu1/viz.R")

games <- readRDS("doubutsu1/lg_states.rds")
hashes <- lapply(games, function(v) sapply(v, hash_state))

mate_in_what <- 3
blind <- FALSE

check_solution <- function(h) {
  gameno <- which(sapply(hashes, function(v) h %in% v))[1]
  cnode <- which(hashes[[gameno]] == h)[1]
  problem <- games[[gameno]][[cnode]]
  solve_game_disp(problem, TRUE)
}

rand_problem <- function(m.min = 2, m.max = 10) {
  h <- sample(names(matein)[matein %in% m.min:m.max], 1)
  print(h)
  check_solution(h)
}

# ## Mate in X puzzles
# 
# h <- names(matein)[sample(which(matein == mate_in_what * 2 + 1), 1)]
# gameno <- which(sapply(hashes, function(v) h %in% v))
# cnode <- which(hashes[[gameno]] == h)[1]
# problem <- games[[gameno]][[cnode]]
# {
#   print_state(problem, blind = blind)
#   if (problem[4] %% 2 == 0) {
#     catn(paste0("** SENTE TO MOVE (mate in ", mate_in_what, ") **"))
#   } else {
#     catn(paste0("** GOTE TO MOVE (mate in ", mate_in_what, ") **"))
#   }
# }

## find the answer:
