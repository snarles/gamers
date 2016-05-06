source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/gen0/corrective.R")
source("doubutsu1/viz.R")
source("doubutsu1/gen0/sourceE.R")
bs <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bg <- readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
nsample = 1; mateXdepth = 5
lessons <- list()
move.limit <- 10

sente <- "old"; gote <- "og"

invP <- function(v, expo = -1) {
  v <- v^expo
  v/sum(v)
}
states <- readRDS("doubutsu1/lg_states.rds")
states <- states[sapply(states, length) > 2]
states <- lapply(states, do.call, what = rbind)
states <- do.call(rbind, states)
hashes <- apply(states, 1, hash_state)
uninds <- match(unique(hashes), hashes)
states <- states[uninds, ]




catn <- function(x) cat(paste0(x, "\n"))

PIECESTRS <- cbind(c(" ", "K","R","B","P"), c(" ", "k", "r", "b", "p"))

LOX <- c("A1", "B1", "C1", 
         "A2", "B2", "C2",
         "A3", "B3", "C3",
         "A4", "B4", "C4"
)

print_state <- function(state, blind = FALSE) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  movev <- state[49:51]
  strs <- rep("", 12)
  pl <- ((state[4] - 1) %% 2) + 1
  for (i in 1:12) {
    str <- PIECESTRS[board[1, i] + 1, board[2, i] + 1]
    if (board[3, i]==1) {
      str <- paste0("+", str)
    } else {
      str <- paste0(" ", str)
    }
    strs[i] <- str
  }
  mat <- t(matrix(strs, nrow = 3))
  hand1st <- ""; hand2st <- ""
  for (i in 1:4) {
    hand1st <- c(hand1st, rep(PIECESTRS[i + 1, 1], hand1[i]))
    hand2st <- c(hand2st, rep(PIECESTRS[i + 1, 2], hand2[i]))
  }
  hand1st <- paste(hand1st, collapse = "")
  hand2st <- paste(hand2st, collapse = "")
  movest <- "begin"
  if (movev[1] != 0) {
    movest <- paste(PIECESTRS[movev[1] + 1, pl],
                    c("**", LOX)[movev[2] + 1],
                    c("**", LOX)[movev[3] + 1],
                    sep = "-")
  }
  #cat("SHOGI 34 STATE: ")
  catn("")
  if (state[2] >= 1000) {
    catn("Sente win!")
  } else if (state[2] <=-1000) {
    catn("Gote win!")
  } else {
    catn("")
  }
  catn(paste0(state[4], ".", movest))
  if (blind) {
    occ <- which(mat!= "  ", TRUE)
    pcs <- mat[mat!="  "]
    pt <- match(sapply(pcs, substr, 2, 2), PIECESTRS)
    pl1 <- (pt < 7)
    ROW <- letters[occ[, 1]]
    COL <- 4 - occ[, 2]
    POS <- paste0(COL, ROW)
    lala <- cbind(pcs, POS)
    rownames(lala) <- rep(" ", nrow(lala))
    colnames(lala) <- c(" ", " ")
    catn("Board:")
    lala <- lala[order(pt), ]
    print(noquote(lala))
    catn(paste0("Sente hand: ", hand1st))
    catn(paste0("Gote hand: ", hand2st))
  }
  if (!blind) {
    catn(paste0("Gote hand: ", hand2st))
    catn("  +--------+ ")
    for (i in 1:4) {
      cat("  |")
      cat(paste(mat[i, ], collapse = " "))
      catn("|")
    }
    catn("  +--------+ ")
    catn(paste0("Sente hand: ", hand1st))
  }
}