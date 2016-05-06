## #SET THIS AS DIRECTORY TO DOBUTSU PROGRAM
## https://dell.tanaka.ecc.u-tokyo.ac.jp/~ktanaka/dobutsushogi/

SOLVER_DIR <- "/home/snarles/Downloads/dobutsu"

catn <- function(x) cat(paste0(x, "\n"))

PIECESTRSjp <- cbind(c(" . ", "+LI", "+KI", "+ZO", "+HI", "+NI"), 
                     c(" . ", "-LI", "-KI", "-ZO", "-HI", "-NI"))

LOXjp <- c("A1", "B1", "C1", 
           "A2", "B2", "C2",
           "A3", "B3", "C3",
           "A4", "B4", "C4"
)

print_state_JP <- function(state, blind = FALSE) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  movev <- state[49:51]
  strs <- rep("", 12)
  pl <- ((state[4] - 1) %% 2) + 1
  for (i in 1:12) {
    str <- PIECESTRSjp[board[1, i] + 1 + board[3, i], board[2, i] + 1]
    strs[i] <- str
  }
  mat <- t(matrix(strs, nrow = 3))
  handst <- paste(c(hand1[c(4,3,2)], hand2[c(4,3,2)]), collapse = "")
  for (i in 1:4) {
    cat(paste(mat[i, ], collapse = ""))
    catn("")
  }
  catn(handst)
  catn(c("-", "+")[pl])
}

# state <- states[300, ]
# print_state(state)
# print_state_JP(state)

solve_state_raw <- function(state, print = FALSE) {
  odir <- getwd()
  setwd(SOLVER_DIR)
  sink("temp.txt")
  print_state_JP(state)
  sink()
  system("./checkState temp.txt 2> out.txt")
  ff <- readLines("out.txt")
  setwd(odir)
  if (print) {
    for (ll in ff) {
      catn(ll)
    }
  }
  ff  
}


ff <- solve_state_raw(states[300, ], TRUE)



t1 <- proc.time()
res <- list()
for (i in 1:nrow(states)) {
  ff <- solve_state_raw(states[i, ])
  res <- c(res, list(ff))
}
proc.time() - t1
saveRDS(res, "analyses.rds")


