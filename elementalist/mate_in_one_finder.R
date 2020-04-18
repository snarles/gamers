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
arr <- simple_sparse_zero_array(dims, mode = "integer")

state0 <- t(c(4, 5,
            4, 1, 4, 5, 6, 5,
            1, 1, 1, 1, 1,
            1))
state0 <- kronecker(t(t(rep(1, 48))), state0)
state0[, 1] <- 1:48
ids <- 1:48

op_move <- function(state1) {
  state1[, 1] <- state1[, 1] + shuriken[state1[, 2]] # move
  state1[state1[, 1] > 48, 1] <- state1[state1[, 1] > 48, 1] - 48
  # collect if not at void
  state1[(state1[, 1] %% 8) != 0, 
         2 + shuriken[state1[(state1[, 1] %% 8) != 0, 1]]] <- 
    state1[(state1[, 1] %% 8) != 0, 
           2 + shuriken[state1[(state1[, 1] %% 8) != 0, 1]]] +1
  state1  
}

op_light <- function(state1) {
  temp <- state1[, 1] # switch positions
  state1[, 1] <- state1[, 2]
  state1[, 2] <- temp
  state1  
}

op_air <- function(state1) {
  state1[, 5] <- state1[, 5] - 1 # use up one
  state1[, 2] <- state1[, 2] + 1 # move op ahead one
  # if at void move yet again
  state1[(state1[, 2] %% 8) == 0, 2] <- state1[(state1[, 2] %% 8) == 0, 2] + 1
  state1[state1[, 2] > 48, 2] <- state1[state1[, 2] > 48, 2] - 48
  state1
}

op_water <- function(state1) {
  state1[, 6] <- state1[, 6] - 1 # use up one
  state1[, 1] <- state1[, 1] + 1 # move ahead one
  # if at void move yet again
  state1[(state1[, 1] %% 8) == 0, 1] <- state1[(state1[, 1] %% 8) == 0, 1] + 1
  state1[state1[, 1] > 48, 1] <- state1[state1[, 1] > 48, 1] - 48
  state1
}

op_earth <- function(state1) {
  # collect if not at void
  state1[(state1[, 1] %% 8) != 0, 
         2 + shuriken[state1[(state1[, 1] %% 8) != 0, 1]]] <- 
    state1[(state1[, 1] %% 8) != 0, 
           2 + shuriken[state1[(state1[, 1] %% 8) != 0, 1]]] +1
  state1
}

use_one_power <- function(state0_ids) {
  state0 <- state0_ids[, 1:14]
  ids <- state0_ids[, 15]
  ans <- matrix(NA, 0, length(dims))
  ans_pred_ids <- numeric()
  # post-movement state
  ids1 <- ids[state0[, 14] != 2]
  state1 <- state0[state0[, 14] != 2, , drop=FALSE]
  state1[, 14] <- 2 # update to indicate post-move
  state1 <- op_move(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using light power
  ids1 <- ids[state0[, 3] != 1]
  state1 <- state0[state0[, 3] != 1, , drop=FALSE]
  state1[, 9] <- 2 # indicate power used
  state1[, 3] <- state1[, 3] - 1 # use up one
  state1 <- op_light(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using air power
  ids1 <- ids[state0[, 5] != 1]
  state1 <- state0[state0[, 5] != 1, , drop=FALSE]
  state1[, 11] <- 2 # indicate power used
  state1[, 5] <- state1[, 5] - 1 # use up one
  state1 <- op_air(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using water power
  ids1 <- ids[state0[, 6] != 1]
  state1 <- state0[state0[, 6] != 1, , drop=FALSE]
  state1[, 12] <- 2 # indicate power used
  state1[, 6] <- state1[, 6] - 1 # use up one
  state1 <- op_water(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using earth power
  ids1 <- ids[state0[, 7] != 1]
  state1 <- state0[state0[, 7] != 1, , drop=FALSE]
  state1[, 13] <- 2 # indicate power used
  state1[, 7] <- state1[, 7] - 1 # use up one
  state1 <- op_earth(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using grass->light
  ids1 <- ids[state0[, 8] != 1 & state0[, 9] == 2]
  state1 <- state0[state0[, 8] != 1, , drop=FALSE]
  state1[, 8] <- state1[, 8] - 1 # use up one
  state1 <- op_light(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using grass->air
  ids1 <- ids[state0[, 8] != 1 & state0[, 11] == 2]
  state1 <- state0[state0[, 8] != 1, , drop=FALSE]
  state1[, 8] <- state1[, 8] - 1 # use up one
  state1 <- op_air(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using grass->water
  ids1 <- ids[state0[, 8] != 1 & state0[, 12] == 2]
  state1 <- state0[state0[, 8] != 1, , drop=FALSE]
  state1[, 8] <- state1[, 8] - 1 # use up one
  state1 <- op_water(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  # using grass->earth
  ids1 <- ids[state0[, 8] != 1 & state0[, 13] == 2]
  state1 <- state0[state0[, 8] != 1, , drop=FALSE]
  state1[, 8] <- state1[, 8] - 1 # use up one
  state1 <- op_earth(state1)
  ans <- rbind(ans, state1)
  ans_pred_ids <- c(ids, ids1)
  colnames(ans) <- cns
  cbind(ans, ans_pred_ids)  
}




ans <- cbind(state0, ids)

arr[state0] <- ids

colnames(ans) <- c(cns, "ans_pred_ids")

res <- use_one_power(ans)
# check uniqueness
id_check <- arr[res[, 1:14]]
res <- res[id_check == 0, , drop = FALSE]
