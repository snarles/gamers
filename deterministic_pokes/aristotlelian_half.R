

track <- c(1,1,2,3,0,
           3,3,1,4,0,
           4,4,3,2,0,
           2,2,4,1,0)
e_pos <- c(1, 0, 0, 0, 0)
e_fire <- c(0, 1, 0, 0, 0)
e_air <- c(0, 0, 1, 0, 0)
e_water <- c(0, 0, 0, 1, 0)
e_earth <- c(0, 0, 0, 0, 1)
i_pos <- 1
i_fire <- 2
i_air <- 3
i_water <- 4
i_earth <- 5
zero_amt <- 1
fire_thres <- zero_amt + 0
MAX_ELTS <- zero_amt + 2

is_win <- function(p1) {
  if (min(p1[2:5]) > zero_amt) {
    return(TRUE)
  }
  return(FALSE)
}

move <- function(p1, n_pos = 1) {
  p1 <- p1 + n_pos * e_pos
  if (p1[1] > 20) p1[1] <- p1[1] - 20
  p1
}

take <- function(p1) {
  if (track[p1[1]] > 0) {
    if (p1[track[p1[1]] + 1] < MAX_ELTS) {
      p1[track[p1[1]] + 1] <- p1[track[p1[1]] + 1] + 1
    }
  }
  p1
}

regular_move <- function(p1, p2) {
  p1 <- move(p1, track[p2[1]])
  p1 <- take(p1)
  p1
}

possible_powers <- function(p1, p2) {
  results <- matrix(c(p1, p2), 1, 10)
  if (p1[i_fire] > zero_amt) {
    if (p2[i_fire] > fire_thres) {
      results <- rbind(results, c(p1 - e_fire, p2 - e_fire))
    }
    if (p2[i_air] > fire_thres) {
      results <- rbind(results, c(p1 - e_fire, p2 - e_air))
    }
    if (p2[i_water] > fire_thres) {
      results <- rbind(results, c(p1 - e_fire, p2 - e_water))
    }
    if (p2[i_earth] > fire_thres) {
      results <- rbind(results, c(p1 - e_fire, p2 - e_earth))
    }
  }
  if (p1[i_air] > zero_amt) {
    results <- rbind(results, c(p1 - e_air, move(p2)))
  }
  if (p1[i_water] > zero_amt) {
    results <- rbind(results, c(move(p1) - e_water, p2))
  }
  if (p1[i_earth] > zero_amt) {
    results <- rbind(results, c(take(p1) - e_earth, p2))
  }
  results
}

iterate_powers <- function(p1, p2) {
  results <- possible_powers(p1, p2)
  n_r <- dim(results)[1]
  flag <- TRUE
  while (flag) {
    flag <- FALSE
    for (i in 1:n_r) {
      results2 <- possible_powers(results[i, 1:5], results[i, 6:10])
      results <- rbind(results, results2)
    }
    results <- unique(results)
    if (dim(results)[1] > n_r) {
      flag <- TRUE
      n_r <- dim(results)[1]
    }
  }
  results
}

p1_turn <- function(p1, p2) {
  results <- iterate_powers(p1, p2)
  for (i in 1:dim(results)[1]) {
    results[i, 1:5] <- regular_move(results[i, 1:5], results[i, 6:10])
  }
  results
}

p2_turn <- function(p1, p2) {
  results <- p1_turn(p2, p1)
  cbind(results[, 6:10, drop=FALSE], results[, 1:5, drop=FALSE])
}

dim_player <- c(length(track), rep(MAX_ELTS, 4))
n_states <- prod(dim_player)^2
state_codes <- array(1:n_states, dim=c(dim_player, dim_player))
compute_successors <- function(state_nos) {
  allresults <- list()
  for (ind in 1:length(state_nos)) {
    state_no <- state_nos[ind]
    state <- which(state_codes == state_no, arr.ind=TRUE)[1,]
    p1 <- state[1:5]
    p2 <- state[6:10]
    results <- p2_turn(p2, p1)
    allresults[[ind]] <- state_codes[results]
  }
  allresults
}

# batch_size <- 729
# batches <- list()
# for (ii in 1:(n_states/batch_size)) {
#   batches[[ii]] <- (ii-1) * batch_size + 1:batch_size
# }
# length(batches)
# 
# library(parallel)

# t1 <- proc.time()
# allres <- mclapply(batches, compute_successors, mc.cores = 80)
# t2 <- proc.time()
# t2 - t1
# 
# successor_states <- unlist(allres, recursive = FALSE)
# 
# # compute_successors(27891)
# # successor_states[27891]
# 
# length(successor_states)
# saveRDS(successor_states, 'deterministic_pokes/arih_ss.rds')

successor_states <- readRDS("deterministic_pokes/arih_ss.rds")

#codes_to_states <- which(state_codes > 0, arr.ind=TRUE)


get_state_values <- function(state_nos) {
  results <- 0 * state_nos
  for (ind in 1:length(state_nos)) {
    state_no <- state_nos[ind]
    state <- codes_to_states[state_no,]
    p1 <- state[1:5]
    p2 <- state[6:10]
    if (is_win(p2) && !is_win(p1)) {
      results[ind] <- -1
    }
    if (is_win(p1) && !is_win(p2)) {
      results[ind] <- 1
    }
  }
  results
}

propagate_values <- function(state_nos) {
  results <- state_values[state_nos]
  for (ind in 1:length(state_nos)) {
    state_no <- state_nos[ind]
    if (state_values[state_no] == 0) {
      ss <- successor_states[[state_no]]
      svs <- state_values[ss]
      if (min(svs) == -1) {
        results[ind] <- 1
      }
      if (min(svs) == 1) {
        results[ind] <- -1
      }
    }
  }
  results
}



# t1 <- proc.time()
# allsv <- mclapply(batches, get_state_values, mc.cores = 80)
# t2 <- proc.time()
# t2 - t1
# 
# state_values <- unlist(allsv, recursive = FALSE)
# sum(state_values != 0)/n_states
# depths <- 0 * state_values - 1
# depths[state_values != 1] <- 0
# 
# t1 <- proc.time()
# depth_count <- 1
# flag <- TRUE
# while(flag) {
#   allsv <- mclapply(batches, propagate_values, mc.cores = 80)
#   new_state_values <- unlist(allsv, recursive = FALSE)
#   depths[state_values != new_state_values] <- depth_count
#   print(c(depth_count, sum(state_values != 0)/n_states))
#   depth_count <- depth_count + 1
#   if (sum(state_values != new_state_values)==0) {
#     flag <- FALSE
#   }
#   state_values <- new_state_values
# }
# t2 <- proc.time()
# t2 - t1
# 
# colnames(codes_to_states) <- c("Loc1", "F1", "A1", "W1", "E1",
#                                "Loc2", "F2", "A2", "W2", "E2")
# 
# tab <- cbind(codes_to_states, state_values, depths)
# saveRDS(tab, "deterministic_pokes/arih_tab.rds")

tab <- readRDS("deterministic_pokes/arih_tab.rds")
utility_vals <- 0 * tab[, 12]
utility_vals[tab[, 11]==1] <- 500 - tab[tab[, 11]==1, 12]
utility_vals[tab[, 11]==-1] <- -(500 - tab[tab[, 11]==-1, 12])
tab <- cbind(tab, utility_vals)

starting_table <- matrix(NA, 20, 20)
for (i in 1:20) {
  for (j in 1:20) {
    starting_table[i, j] <- utility_vals[state_codes[i, 1, 1, 1, 1, j, 1, 1, 1, 1]]
  }
}

p2_optimal_loc <- apply(starting_table, 1, function(i) order(i)[1])

state <- rep(1, 10)
# p1 initial loc
state[1] <- 1
# p2 optimal response to p1 loc
state[6] <- p2_optimal_loc[state[1]]

next_state <- function(state, player = 1) {
  if (player == 2) {
    state <- c(state[6:10], state[1:5])
  }
  ss <- successor_states[[state_codes[t(state)]]]
  uvs <- tab[, 13][ss]
  next_s <- tab[ss[order(uvs)[1]], 1:10]
  if (player != 2) {
    next_s <- c(next_s[6:10], next_s[1:5])
  }
  next_s
}

print_game <- function(state, player = 1) {
  while(tab[, 12][state_codes[t(state)]] != 0) {
    state <- next_state(state, player)
    player <- 3 - player
    print(state)
  }
}


