

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
n_states <- 2 * prod(dim_player)^2
state_codes <- array(1:n_states, dim=c(2, dim_player, dim_player))

compute_successors <- function(state_nos) {
  allresults <- list()
  for (ind in 1:length(state_nos)) {
    state_no <- state_nos[ind]
    state <- which(state_codes == state_no, arr.ind=TRUE)[1,]
    curr <- state[1]
    p1 <- state[2:6]
    p2 <- state[7:11]
    
    if (curr == 1) {
      results <- cbind(2, p1_turn(p1, p2))
    }
    if (curr == 2) {
      results <- cbind(1, p2_turn(p1, p2))
    }
    allresults[[ind]] <- state_codes[results]
  }
  allresults
}


successor_states <- list(n_states)
#for (state_no in 1:n_states) {
# t1 <- proc.time()
# smp <- sample(n_states, 500)
# stuff <- compute_successors(smp)
# t2 <- proc.time()
# t2 - t1

batch_size <- 800
batches <- list()
for (ii in 1:(n_states/batch_size)) {
  batches[[ii]] <- (ii-1) * batch_size + 1:batch_size
}

library(parallel)
t1 <- proc.time()
allres <- mclapply(batches[1:729], compute_successors, mc.cores = 80)
t2 <- proc.time()
t2 - t1

successor_states <- unlist(allres, recursive = FALSE)

compute_successors(27891)
successor_states[27891]

length(successor_states)
saveRDS(successor_states, 'deterministic_pokes/ari_ss_1.rds')
