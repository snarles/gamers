####
## Tic-Tac-Ko
####
# A variant of Tic-Tac-Toe
#  where you can either erase an opponent's mark or place your own,
#  but you can't repeat a previous game state

display_state <- function(statevec) {
  chars <- c(" ", "X", "O")
  for (i in 1:3) {
    for (j in 1:3) {
      cat(chars[statevec[(i-1)*3 + j]])
    }
    cat('\n')
  }
}

win_patterns <- rbind(
  c(1,2,3), # rows
  c(4,5,6),
  c(7,8,9),
  c(1,4,7), # columns
  c(2,5,8),
  c(3,6,9),
  c(1,5,9), # diagonals
  c(3,5,7)
)
# number of win patterns
nwp <- dim(win_patterns)[1]


####
##  Construct set of valid states
####

## unfiltered quantities
# number of states
ns <- 3^9
# states to code
s2c <- array(1:ns, dim=rep(3, 9))
# codes to state
c2s <- which(s2c > 0, TRUE)
# find states where p1/p2 has won
win_p1 <- rowSums(sapply(1:nwp, function(i) rowSums(c2s[, win_patterns[i, ]]==2) == 3)) > 0
win_p2 <- rowSums(sapply(1:nwp, function(i) rowSums(c2s[, win_patterns[i, ]]==3) == 3)) > 0
# eliminate invalid states
sum_p1 <- rowSums(c2s==2)
sum_p2 <- rowSums(c2s==3)
valid <- (!(win_p1 & win_p2)) & ((sum_p2 == (sum_p1-1)) | (sum_p2==sum_p1))
sum(sum_p2 > sum_p1)
valid_codes <- which(valid)
## update quantities filtered by validity
ns <- length(valid_codes)
c2s <- c2s[valid_codes, ]
s2c <- array(0, dim=rep(3,9))
s2c[c2s] <- 1:ns
win_p1 <- rowSums(sapply(1:nwp, function(i) rowSums(c2s[, win_patterns[i, ]]==2) == 3)) > 0
win_p2 <- rowSums(sapply(1:nwp, function(i) rowSums(c2s[, win_patterns[i, ]]==3) == 3)) > 0
stopifnot(sum(win_p1 & win_p2)==0)
rm(valid_codes)
rm(valid)
# player to move can be determined by sums
sum_p1 <- rowSums(c2s==2)
sum_p2 <- rowSums(c2s==3)
p1_to_play <- (sum_p1==sum_p2)
p2_to_play <- (sum_p1==(sum_p2+1))
stopifnot(sum(p1_to_play == p2_to_play)==0)
# no moves are possible from terminal states
game_over <- win_p1 | win_p2

####
##  Compute successor and predecessor states
####

successors <- matrix(0, ns, 9)
predecessors <- matrix(0, ns, 9)

# P1 moves
for (move_loc in 1:9) {
  # place
  next_states <- c2s
  next_states[, move_loc] <- 2
  valid_filt <- (c2s[, move_loc]==1) & p1_to_play & !game_over
  state_codes <- s2c[next_states[valid_filt, ]]
  stopifnot(sum(state_codes==0)==0)
  successors[valid_filt, move_loc] <- state_codes
  predecessors[state_codes, move_loc] <- which(valid_filt)
  # erase
  next_states <- c2s
  next_states[, move_loc] <- 1
  valid_filt <- (c2s[, move_loc]==3) & p1_to_play & !game_over
  state_codes <- s2c[next_states[valid_filt, ]]
  stopifnot(sum(state_codes==0)==0)
  successors[valid_filt, move_loc] <- state_codes
  predecessors[state_codes, move_loc] <- which(valid_filt)
}
# P2 moves
for (move_loc in 1:9) {
  # place
  next_states <- c2s
  next_states[, move_loc] <- 3
  valid_filt <- (c2s[, move_loc]==1) & p2_to_play & !game_over
  state_codes <- s2c[next_states[valid_filt, ]]
  stopifnot(sum(state_codes==0)==0)
  successors[valid_filt, move_loc] <- state_codes  
  predecessors[state_codes, move_loc] <- which(valid_filt)
  # erase
  next_states <- c2s
  next_states[, move_loc] <- 1
  valid_filt <- (c2s[, move_loc]==2) & p2_to_play & !game_over
  state_codes <- s2c[next_states[valid_filt, ]]
  stopifnot(sum(state_codes==0)==0)
  successors[valid_filt, move_loc] <- state_codes
  predecessors[state_codes, move_loc] <- which(valid_filt)
}

mean(rowSums(successors!=0)) # 5.017317 is average branching factor

# rc <- sample(ns, 1)
# display_state(c2s[rc, ])
# for (ss in successors[rc,]) {
#   if (ss != 0) {
#     cat('\n')
#     display_state(c2s[ss, ])
#   }
# }

####
##  Find winning states (no mate-in-1 states exist)
####

depths <- numeric(ns) - 1
winners <- numeric(ns) + 2 * win_p1 + 3 * win_p2
depths[winners != 0] <- 0
mean(depths != -1)
depth_counter <- 1

temp <- matrix(c(-1, winners)[successors+1], ncol=9)
sum0 <- rowSums(temp==0)
sum2 <- rowSums(temp==2)
sum3 <- rowSums(temp==3)
p1_win_next <- (p1_to_play & (sum2 > 0)) | (p2_to_play & ((sum0 + sum3)==0))
p2_win_next <- (p2_to_play & (sum3 > 0)) | (p1_to_play & ((sum0 + sum2)==0))
new_states <- (p1_win_next | p2_win_next) & (depths==-1)
depths[new_states] <- depth_counter
winners[p1_win_next & new_states] <- 2
winners[p2_win_next & new_states] <- 3
print(mean(depths != -1))
depth_counter <- depth_counter + 1

# display_state(c2s[sample(which(depths==1 & p2_to_play), 1),])

table(winners, depths)/ns

####
## Compute utilities under random play
####

# initialize utilities to be 1 for respective player winning states
uts <- numeric(ns)
uts[p1_to_play] <- 0.5
uts[p2_to_play] <- 0.5
uts[winners==2 & p1_to_play] <- 1
uts[winners==3 & p2_to_play] <- 1

# only update utlities of non-winning states
nwf <- (winners==0)
nsuc <- rowSums(successors > 0)

# update loop
for (i in 1:30) {
  old_uts <- uts
  uts[nwf] <- rowSums(matrix(c(0, (1-uts))[successors[nwf, ] + 1], ncol=9))/nsuc[nwf]
  print(mean(abs(old_uts - uts)))  
}

mean(uts[p1_to_play]) # 0.7903615
mean(uts[p2_to_play]) # 0.5277195
del_z <- function(v) v[v > 0]

rs <- sample(which(nwf), 1)
display_state(c2s[rs, ])
ss <- del_z(successors[rs, ])
utss <- uts[ss]
for (i in order(utss)) {
  display_state(c2s[ss[i], ])
  print(utss[i])
  cat('\n')
}
