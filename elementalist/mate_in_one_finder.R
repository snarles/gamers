####
##  List all the boundary states that are mate-in-one
##  (which is to say, all states with more resources are also mate-in-one)
####
# Game state indices
# 1-2: P1 location
#  add 2 to get elt counter (note that counter starts at 1)
# 3: L
# 4: F
# 5: A
# 6: W
# 7: E
# 8: G
#  add 8 to get elt used
# 9: L used (2 = used)
# 10: F used
# 11: A used
# 12: W used
# 13: E used
# 14: regular move made (2 = yes)

shuriken <- c(
  1, 1, 2, 3, 5, 1, 6, 0, 
  5, 5, 3, 1, 4, 5, 2, 0, 
  4, 4, 1, 5, 6, 4, 3, 0, 
  6, 6, 5, 4, 2, 6, 1, 0, 
  2, 2, 4, 6, 3, 2, 5, 0, 
  3, 3, 6, 2, 1, 3, 4, 0
)

library(slam)
MAX_ELT <- 20
dims <- c(48, 48, rep(MAX_ELT, 6), rep(2, 5), 2)
cns <- c("l1", "l2", "L", "F", "A", "W", "E", "G", 
         "Lu", "Fu", "Au", "Wu", "Eu", "Mvd")

op_move <- function(state1) {
  state1[, 1] <- state1[, 1] + shuriken[state1[, 2]] # move
  state1[state1[, 1] > 48, 1] <- state1[state1[, 1] > 48, 1] - 48
  # collect if not at void
  state1 <- op_collect(state1)
  state1  
}

op_collect <- function(state1) {
  eltype <- shuriken[state1[, 1]]
  for (i in 1:6) {
    state1[eltype==i, 2+i] <- state1[eltype==i, 2+i]+1
  }
  state1
}

op_light <- function(state1) {
  temp <- state1[, 1] # switch positions
  state1[, 1] <- state1[, 2]
  state1[, 2] <- temp
  state1  
}

op_air <- function(state1) {
  state1[, 2] <- state1[, 2] + 1 # move op ahead one
  # if at void move yet again
  state1[(state1[, 2] %% 8) == 0, 2] <- state1[(state1[, 2] %% 8) == 0, 2] + 1
  state1[state1[, 2] > 48, 2] <- state1[state1[, 2] > 48, 2] - 48
  state1
}

op_water <- function(state1) {
  state1[, 1] <- state1[, 1] + 1 # move ahead one
  # if at void move yet again
  state1[(state1[, 1] %% 8) == 0, 1] <- state1[(state1[, 1] %% 8) == 0, 1] + 1
  state1[state1[, 1] > 48, 1] <- state1[state1[, 1] > 48, 1] - 48
  state1
}

op_earth <- op_collect

actions <- c('Mv','L','A','W','E','Gl','Ga','Gw','Ge')
# expands states and returns table of successors
build_successors <- function(states, rri, state0, successors) {
  check_id <- rri[state0]
  new_ids <- (check_id==0)
  rri[state0[new_ids, , drop=FALSE]] <- nrow(states) + 1:sum(new_ids)
  ids0 <- rri[state0]
  states <- rbind(states, state0[new_ids, , drop=FALSE])
  if (nrow(successors) < nrow(states)) {
    successors <- rbind(successors, 
                        matrix(0, nrow(states) - nrow(successors), length(actions)))
  }
  for (action_ind in 1:length(actions)) {
    action <- actions[action_ind]
    cat(action)
    if (action=='Mv') {
      ids1 <- ids0[state0[, 14] != 2]
      state1 <- state0[state0[, 14] != 2, , drop=FALSE]
      state1[, 14] <- 2 # update to indicate post-move
      state1 <- op_move(state1)
    }
    if (action=='L') {
      ids1 <- ids0[state0[, 3] != 1]
      state1 <- state0[state0[, 3] != 1, , drop=FALSE]
      state1[, 9] <- 2 # indicate power used
      state1[, 3] <- state1[, 3] - 1 # use up one
      state1 <- op_light(state1)
    }
    if (action=='A') {
      ids1 <- ids0[state0[, 5] != 1]
      state1 <- state0[state0[, 5] != 1, , drop=FALSE]
      state1[, 11] <- 2 # indicate power used
      state1[, 5] <- state1[, 5] - 1 # use up one
      state1 <- op_air(state1)
    }
    if (action=='W') {
      ids1 <- ids0[state0[, 6] != 1]
      state1 <- state0[state0[, 6] != 1, , drop=FALSE]
      state1[, 12] <- 2 # indicate power used
      state1[, 6] <- state1[, 6] - 1 # use up one
      state1 <- op_water(state1)
    }
    if (action=='E') {
      ids1 <- ids0[state0[, 7] != 1]
      state1 <- state0[state0[, 7] != 1, , drop=FALSE]
      state1[, 13] <- 2 # indicate power used
      state1[, 7] <- state1[, 7] - 1 # use up one
      state1 <- op_earth(state1)
    }
    if (action %in% c('Gl','Ga','Gw','Ge')) {
      ids1 <- ids0[state0[, 8] != 1]
      state1 <- state0[state0[, 8] != 1, , drop=FALSE]
      state1[, 8] <- state1[, 8] - 1 # use up one
      if (action=='Gl') state1 <- op_light(state1)
      if (action=='Ga') state1 <- op_air(state1)
      if (action=='Gw') state1 <- op_water(state1)
      if (action=='Ge') state1 <- op_earth(state1)
    }
    s1_ids <- rri[state1]
    # assign new ids to states with index 0 and add them to states
    new_s1 <- (s1_ids == 0)
    rri[state1[new_s1, , drop = FALSE]] <- nrow(states) + 1:sum(new_s1)
    states <- rbind(states, state1[new_s1, , drop = FALSE])
    s1_ids <- rri[state1]
    # print(dim(states))
    # update successors
    successors[ids1, action_ind] <- s1_ids
  } # end for loop over actions
  list(states = states, rri = rri, successors = successors, state0 = state0)
}



# index array
rri <- simple_sparse_zero_array(dims, mode = "integer")
# value array, 1 = Mate-in-1, 0 = unknown, -1 = No mate in 1
rrv <- simple_sparse_zero_array(dims, mode = "integer")
state_init <- t(c(4, 5,
              2, 1, 1, 2, 2, 2,
              1, 1, 1, 1, 1,
              1))
states <- matrix(0, 0, length(dims))
successors <- matrix(0, 0, length(actions))

state0 <- kronecker(t(t(rep(1, 48))), state_init)
res <- build_successors(states, rri, state0, successors)

# loop
flag <- TRUE
while(flag) {
  prev_size <- nrow(states)
  states <- res$states
  rri <- res$rri
  successors <- res$successors
  if (nrow(states) > prev_size) {
    successors <- rbind(successors, matrix(0, nrow(states)-prev_size, 9))
    state0 <- states[(prev_size+1):nrow(states), , drop=FALSE]
    res <- build_successors(states, rri, state0, successors)
  }
  else {
    flag <- FALSE
  }
}
dim(states)
dim(successors)
table(rowSums(states[, 3:8] - 1))
