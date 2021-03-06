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

rc <- sample(ns, 1)
display_state(c2s[rc, ])
for (ss in successors[rc,]) {
  if (ss != 0) {
    cat('\n')
    display_state(c2s[ss, ])
  }
}

####
##  Find winning states and losing states (no mate-in-1 exist)
####

depths <- numeric(ns) - 1
winners <- numeric(ns) + 2 * win_p1 + 3 * win_p2
depths[winners != 0] <- 0
mean(depths != -1)
depth_counter <- 1

for (iii in 1:3) {
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
}
max(depths) # 2
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
uts[winners==2 & p2_to_play] <- 0
uts[winners==3 & p1_to_play] <- 0

# only update utlities of non-winning/losing states
nwf <- (winners==0)
nsuc <- rowSums(successors > 0)

# update loop
for (i in 1:30) {
  old_uts <- uts
  uts[nwf] <- rowSums(matrix(c(0, (1-uts))[successors[nwf, ] + 1], ncol=9))/nsuc[nwf]
  print(mean(abs(old_uts - uts)))  
}

mean(uts[p1_to_play]) # 0.738643
mean(uts[p2_to_play]) # 0.4173138
del_z <- function(v) v[v > 0]

# rs <- sample(which(nwf), 1)
# display_state(c2s[rs, ])
# ss <- del_z(successors[rs, ])
# utss <- uts[ss]
# for (i in order(utss)) {
#   display_state(c2s[ss[i], ])
#   print(utss[i])
#   cat('\n')
# }

####
##  Third reduction: keep only non-winning states
####

nwf <- !((winners==2 & p1_to_play) | (winners==3 & p2_to_play))
nwf_index <- which(nwf)
rev_nwf <- numeric(ns)
rev_nwf[nwf_index] <- 1:length(nwf_index)
uts <- uts[nwf]
c2s <- c2s[nwf, ]
ns <- length(nwf_index) # 3120
s2c <- array(0, dim=rep(3,9))
s2c[c2s] <- 1:ns
sum_p1 <- rowSums(c2s==2)
sum_p2 <- rowSums(c2s==3)
p1_to_play <- (sum_p1==sum_p2)
p2_to_play <- (sum_p1==(sum_p2+1))
stopifnot(sum(p1_to_play == p2_to_play)==0)

temp <- matrix(c(-1, winners)[successors+1], ncol=9)
successors2 <- successors
successors2[temp > 1] <- 0
successors2 <- matrix(c(0, rev_nwf)[successors2+1], ncol=9)
successors <- successors2[nwf, ]

mean(rowSums(successors != 0)) # 2.598077 is pruned branching factor

rs <- sample(ns, 1)
display_state(c2s[rs, ])
ss <- del_z(successors[rs, ])
utss <- uts[ss]
for (i in order(utss)) {
  display_state(c2s[ss[i], ])
  print(utss[i])
  cat('\n')
}

####
##  Generate one play sequence
####


ordered_succ <- lapply(1:ns, function(rs) {
  ss <- del_z(successors[rs, ])
  ss[order(uts[ss])]
})
get_succ <- function(sseq) {
  rs <- sseq[length(sseq)]
  ss <- ordered_succ[[rs]]
  setdiff(ss, sseq)
}
get_succ_sseq <- function(sseq) {
  rs <- sseq[length(sseq)]
  ss <- ordered_succ[[rs]]
  pmoves <- setdiff(ss, sseq)
  lapply(pmoves, function(i) c(sseq, i))
}
eval_sseq <- function(sseq, depth=0) {
  succ <- get_succ_sseq(sseq)
  if (length(succ) == 0) {
    return(-1)
  }
  if (depth == 0) return(0)
  cvs <- sapply(succ, eval_sseq, depth=depth-1)
  if (min(cvs) == -1) return(1)
  if (min(cvs) == 1) return(-1)
  return(0)
}
next_sseq <- function(sseq, depth=0) {
  succ <- get_succ_sseq(sseq)
  if (length(succ)==0) {
    return(sseq)
  }
  if (depth==0) {
    return(succ[[1]])
  }
  cvs <- sapply(succ, eval_sseq2, depth=depth)
  succ[[which(cvs==min(cvs))[1]]]
}
eval_sseq2 <- function(sseq, depth=0) {
  succ <- get_succ_sseq(sseq)
  if (length(succ) == 0) {
    return(-1000)
  }
  if (depth == 0) return(0)
  cvs <- sapply(succ, eval_sseq2, depth=depth-1)
  if (min(cvs) < 0) return(-min(cvs) - 1)
  if (min(cvs) > 0) return(-min(cvs) + 1)
  return(0)
}



####
## Playing with different depths
####

ai_vs_ai <- function(p1depth, p2depth, sseq=1) {
  while(length(get_succ_sseq(sseq)) > 0) {
    display_state(c2s[sseq[length(sseq)],])
    cat('\n')
    if (p1_to_play[sseq[length(sseq)]]) {
      sseq <- next_sseq(sseq, p1depth)
    }
    else {
      sseq <- next_sseq(sseq, p2depth)
    }
  }
  display_state(c2s[sseq[length(sseq)],])
  cat('\n')
  return(sseq)
}

sseq <- ai_vs_ai(0, 0)
length(sseq) # 9
p2_to_play[sseq[length(sseq)]] # p2 wins

sseq <- ai_vs_ai(1, 0)
length(sseq) # 14
p2_to_play[sseq[length(sseq)]] # p1 wins

sseq <- ai_vs_ai(1, 1)
length(sseq) # 32
p2_to_play[sseq[length(sseq)]] # p1 wins

sseq <- ai_vs_ai(1, 2)
length(sseq) # 32
p2_to_play[sseq[length(sseq)]] # p1 wins

sseq <- ai_vs_ai(1, 3)
length(sseq) # 84
p2_to_play[sseq[length(sseq)]] # p1 wins

sseq <- ai_vs_ai(1, 4)
length(sseq) # 84
p2_to_play[sseq[length(sseq)]] # p1 wins

sseq <- ai_vs_ai(1, 5)
length(sseq) # 52
p2_to_play[sseq[length(sseq)]] # p1 wins

####
##  Conclusion: with very slight amount of skill, P1 appears unbeatable
##  Next, investigate restricting the opening move
####

sseq <- ai_vs_ai(0, 0, c(1, 3))
length(sseq) # 9
p2_to_play[sseq[length(sseq)]] # p2 wins

sseq <- ai_vs_ai(1, 0, c(1, 3))
length(sseq) # 12
p2_to_play[sseq[length(sseq)]] # p1 wins

sseq <- ai_vs_ai(1, 5, c(1, 3))
length(sseq) # 64
p2_to_play[sseq[length(sseq)]] # p1 wins

####
##  Even forcing P1 to use the worst opening move is not sufficient to balance the game
##  The only way to balance may to impose a move limit, after which P2 wins
####

# depths <- which(matrix(TRUE, 5, 5), TRUE) - 1
# colnames(depths) <- c("p1depth", "p2depth")
# length_and_winner <- t(apply(depths, 1, function(v) {
#   sseq <- ai_vs_ai(v[1], v[2])
#   c(length(sseq), p2_to_play[sseq[length(sseq)]])
# }))
# colnames(length_and_winner) <- c("length", "p1win")
# cbind(depths, length_and_winner)

#      p1depth p2depth length p1win
# [1,]       0       0      9     0
# [2,]       1       0     14     1
# [3,]       2       0     14     1
# [4,]       3       0     14     1
# [5,]       4       0     14     1
# [6,]       0       1      9     0
# [7,]       1       1     32     1
# [8,]       2       1     20     1
# [9,]       3       1     20     1
# [10,]       4       1     18     1
# [11,]       0       2      9     0
# [12,]       1       2     32     1
# [13,]       2       2     20     1
# [14,]       3       2     20     1
# [15,]       4       2     18     1
# [16,]       0       3      9     0
# [17,]       1       3     84     1
# [18,]       2       3     20     1
# [19,]       3       3     20     1
# [20,]       4       3     18     1
# [21,]       0       4      9     0
# [22,]       1       4     84     1
# [23,]       2       4     20     1
# [24,]       3       4     20     1
# [25,]       4       4     18     1


####
##  Play vs CPU
####

display_game <- function(sseq, ncols = 8){
  states <- c2s[sseq, , drop=FALSE]
  chars <- c(" ", "X", "O")
  for (iii in 1:ceiling(length(sseq)/ncols)) {
    sub_states <- states[1:min(ncols, dim(states)[1]), , drop=FALSE]
    cat(paste0(rep('  --- ', dim(sub_states)[1])))
    cat('\n')
    for (i in 1:3) {
      rr <- sub_states[, (i-1)*3 + 1:3, drop=FALSE]
      for (j in 1:dim(rr)[1]) {
        cat(" |")
        cat(paste0(chars[rr[j, ]], collapse=''))
        cat("| ")
      }
      cat('\n')
    }
    cat(paste0(rep('  --- ', dim(sub_states)[1])))
    cat('\n')
    states <- states[-(1:min(ncols, dim(states)[1])), , drop=FALSE]
  }
}
display_game(sseq)

play_vs_ai <- function(aidepth, p1_plays=TRUE, sseq=1) {
  while(length(get_succ_sseq(sseq)) > 0) {
    display_game(sseq)
    cat('\n')
    readline("Paused:")
    if (p1_to_play[sseq[length(sseq)]]==p1_plays) {
      flag <- TRUE
      while (flag) {
        choice <- as.numeric(readline('Next move (1-9):'))
        rs <- sseq[length(sseq)]
        choices <- get_succ_sseq(sseq)
        succ <- get_succ(sseq)
        if (length(succ)==0) {
          cat('----YOU LOSE----\n')
          return()
        }
        if (is.element(successors[rs, choice], succ)) {
          flag <- FALSE
        }
        else {
          cat("--INVALID MOVE--\n")
        }
      }
      sseq <- c(sseq, successors[rs, choice])
    }
    else {
      sseq <- next_sseq(sseq, aidepth)
    }
  }
  display_game(sseq)
  if (p2_to_play[sseq[length(sseq)]]==p1_plays) cat('---YOU WIN!---\n')
  if (p1_to_play[sseq[length(sseq)]]==p1_plays) cat('---YOU LOSE---\n')
  sseq
}
