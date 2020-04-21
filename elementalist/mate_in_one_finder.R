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

shuriken <- as.integer(c(
  1, 1, 2, 3, 5, 1, 6, 0, 
  5, 5, 3, 1, 4, 5, 2, 0, 
  4, 4, 1, 5, 6, 4, 3, 0, 
  6, 6, 5, 4, 2, 6, 1, 0, 
  2, 2, 4, 6, 3, 2, 5, 0, 
  3, 3, 6, 2, 1, 3, 4, 0
))

library(slam)
MAX_ELT <- 20L
dims <- as.integer(c(48, 48, rep(MAX_ELT, 6), rep(2, 5), 2))
cns <- c("l1", "l2", "L", "F", "A", "W", "E", "G", 
         "Lu", "Fu", "Au", "Wu", "Eu", "Mvd")

op_move <- function(state1) {
  state1[, 1] <- state1[, 1] + shuriken[state1[, 2]] # move
  state1[state1[, 1] > 48, 1] <- state1[state1[, 1] > 48, 1] - 48L
  # collect if not at void
  state1 <- op_collect(state1)
  state1  
}

op_collect <- function(state1) {
  eltype <- shuriken[state1[, 1]]
  for (i in 1:6) {
    state1[eltype==i, 2+i] <- state1[eltype==i, 2+i]+1L
  }
  state1[state1 > MAX_ELT] <- MAX_ELT
  state1
}

op_light <- function(state1) {
  temp <- state1[, 1] # switch positions
  state1[, 1] <- state1[, 2]
  state1[, 2] <- temp
  state1  
}

op_air <- function(state1) {
  state1[, 2] <- state1[, 2] + 1L # move op ahead one
  # if at void move yet again
  state1[(state1[, 2] %% 8) == 0, 2] <- state1[(state1[, 2] %% 8) == 0, 2] + 1L
  state1[state1[, 2] > 48, 2] <- state1[state1[, 2] > 48, 2] - 48L
  state1
}

op_water <- function(state1) {
  state1[, 1] <- state1[, 1] + 1L # move ahead one
  # if at void move yet again
  state1[(state1[, 1] %% 8) == 0, 1] <- state1[(state1[, 1] %% 8) == 0, 1] + 1L
  state1[state1[, 1] > 48, 1] <- state1[state1[, 1] > 48, 1] - 48L
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
                        matrix(0L, nrow(states) - nrow(successors), length(actions)))
  }
  stopifnot(nrow(states)==length(unique(rri[states])))
  for (action_ind in 1:length(actions)) {
    action <- actions[action_ind]
    cat(action)
    if (action=='Mv') {
      ids1 <- ids0[state0[, 14] != 2]
      state1 <- state0[state0[, 14] != 2, , drop=FALSE]
      state1[, 14] <- 2L # update to indicate post-move
      # to reduce redundancy
      state1[, 9:13] <- 1L
      state1 <- op_move(state1)
    }
    if (action=='L') {
      filt <- (state0[, 14] != 2) & (state0[, 3] != 1)
      ids1 <- ids0[filt]
      state1 <- state0[filt, , drop=FALSE]
      state1[, 9] <- 2L # indicate power used
      state1[, 3] <- state1[, 3] - 1L # use up one
      state1 <- op_light(state1)
    }
    if (action=='A') {
      filt <- (state0[, 14] != 2) & (state0[, 5] != 1)
      ids1 <- ids0[filt]
      state1 <- state0[filt, , drop=FALSE]
      state1[, 11] <- 2L # indicate power used
      state1[, 5] <- state1[, 5] - 1L # use up one
      state1 <- op_air(state1)
    }
    if (action=='W') {
      filt <- (state0[, 14] != 2) & (state0[, 6] != 1)
      ids1 <- ids0[filt]
      state1 <- state0[filt, , drop=FALSE]
      state1[, 12] <- 2L # indicate power used
      state1[, 6] <- state1[, 6] - 1L # use up one
      state1 <- op_water(state1)
    }
    if (action=='E') {
      filt <- (state0[, 14] != 2) & (state0[, 7] != 1)
      ids1 <- ids0[filt]
      state1 <- state0[filt, , drop=FALSE]
      state1[, 13] <- 2L # indicate power used
      state1[, 7] <- state1[, 7] - 1L # use up one
      state1 <- op_earth(state1)
    }
    if (action %in% c('Gl','Ga','Gw','Ge')) {
      filt <- (state0[, 14] != 2) & (state0[, 8] != 1)
      ids1 <- ids0[filt]
      state1 <- state0[filt, , drop=FALSE]
      state1[, 8] <- state1[, 8] - 1L # use up one
      if (action=='Gl') state1 <- op_light(state1)
      if (action=='Ga') state1 <- op_air(state1)
      if (action=='Gw') state1 <- op_water(state1)
      if (action=='Ge') state1 <- op_earth(state1)
    }
    # delete all states that go below total 4
    ids1 <- ids1[rowSums(state1[, 3:8] - 1) >= 4]
    state1 <- state1[rowSums(state1[, 3:8] - 1) >= 4, , drop=FALSE]
    s1_ids <- rri[state1]
    # assign new ids to states with index 0 and add them to states
    new_s1 <- (s1_ids == 0)
    rri[state1[new_s1, , drop = FALSE]] <- nrow(states) + 1:sum(new_s1)
    stopifnot(sum(rri[states] != 1:nrow(states))==0)
    s1_ids <- rri[state1]
    # delete duplicates
    state1x <- unique(state1)
    state1x <- state1x[rri[state1x] > nrow(states), , drop=FALSE]
    rri[state1x] <- nrow(states) + 1:nrow(state1x)
    stopifnot(sum(rri[states] != 1:nrow(states))==0)
    s1_ids <- rri[state1]
    states <- rbind(states, state1x)
    stopifnot(nrow(states)==length(unique(rri[states])))
    # print(dim(states))
    # update successors
    successors[ids1, action_ind] <- s1_ids
  } # end for loop over actions
  successors[successors == (1:nrow(successors))] <- 0
  list(states = states, rri = rri, successors = successors, state0 = state0)
}



# index array
rri <- simple_sparse_zero_array(dims, mode = "integer")
# value array, 1 = Mate-in-1, 0 = unknown, -1 = No mate in 1
rrv <- simple_sparse_zero_array(dims, mode = "integer")

state_init <- t(c(4,15, 2,1,3,3,3,3, 1,1,1,1,1, 1))
state0 <- kronecker(t(t(rep(1L, 48^2))), state_init)
state0[, 1:2] <- which(matrix(TRUE, 48, 48), TRUE)
# state0 <- t(c(4,15, 2,1,3,3,3,3, 1,1,1,1,1, 1))
states <- matrix(0L, 0, length(dims))
successors <- matrix(0L, 0, length(actions))
res <- build_successors(states, rri, state0, successors)



# for (iii in 1:2) {
  size_at_prev_stage <- nrow(states)
  # loop
  flag <- TRUE
  depth_count <- 0
  while(flag) {
    # update states, successors and index
    depth_count <- depth_count + 1
    prev_size <- nrow(states)
    states <- res$states
    rri <- res$rri
    successors <- res$successors
    
    if (nrow(states) > prev_size) {
      state0 <- states[(prev_size+1):nrow(states), , drop=FALSE]
      res <- build_successors(states, rri, state0, successors)
    }
    else {
      flag <- FALSE
    }
  }
  
  # update values (1/2) - assigning values to terminal states
  new_states <- c(rep(FALSE, size_at_prev_stage), rep(TRUE, nrow(states) - size_at_prev_stage))
  term_filt <- (states[, 14]==2)
  win_filt <- (apply(states[, 3:8], 1, min)==2)
  rrv[states[term_filt & win_filt, , drop=FALSE]] = 1
  rrv[states[term_filt & !win_filt, , drop=FALSE]] = -1
  # update values (1/2) - propagating values
  flag2 <- TRUE
  while(flag2) {
    unassigned_filt <- (rrv[states] == 0) & new_states
    if (sum(unassigned_filt) == 0) {
      flag2 <- FALSE
    }
    else {
      values_mat <- matrix(
        c(-1, rrv[states])[successors[unassigned_filt, , drop=FALSE] + 1],
        ncol=length(actions))
      value_1 <- (apply(values_mat, 1, max)==1)
      value_neg <- (apply(values_mat, 1, max)==-1)
      rrv[states[unassigned_filt, , drop=FALSE][value_1, , drop=FALSE]] = 1
      rrv[states[unassigned_filt, , drop=FALSE][value_neg, , drop=FALSE]] = -1
    }
  }
  stopifnot(sum(rrv[states] == 0)==0)
#   # create next states to explore
#   init_states <- (apply(states[,9:14], 1, max)==1)
#   state_c <- states[init_states & new_states & rrv[states]==-1, , drop=FALSE]
#   # increase each elt by 1
#   mod_column <- function(a, i, v=1) {
#     a[, i] <- a[, i] + v
#     a
#   }
#   state0 <- rbind(mod_column(state_c, 3),
#                   mod_column(state_c, 4),
#                   mod_column(state_c, 5),
#                   mod_column(state_c, 6),
#                   mod_column(state_c, 7),
#                   mod_column(state_c, 8))
#   # delete with over max_elt
#   state0 <- state0[apply(state0[, 3:8], 1, max) <= MAX_ELT, , drop=FALSE]
#   state0 <- state0[state0[, 4] <= 2, ,drop=FALSE]
#   # delete winning states
#   state0 <- state0[apply(state0[, 3:8], 1, min) == 1, , drop=FALSE]
#   state0 <- unique(state0)
#   # put states into it
#   res <- build_successors(states, rri, state0, successors)
# }

dim(states)
dim(successors)
table(rowSums(states[, 3:8] - 1))
table(rrv[states])
length(unique(rri[states]))
max(rri[states])

