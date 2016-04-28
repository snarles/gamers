source("doubutsu1/lg_analysis_setup.R")
lmoves <- readRDS("doubutsu1/lmoves.rds")
gstates <- readRDS("doubutsu1/lg_states.rds")
allmoves <- setdiff(sort(unique(do.call(c, glist))), "resign")
nmoves <- length(allmoves)

library(glmnet)

print_state(state)

state <- prev

eye11 <- pracma::eye(11)
ut3 <- pracma::eye(3); ut3[upper.tri(ut3)] <- 1

expand_state <- function(state) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  ptypes <- board[1, ] + board[2, ] * 5 + board[3, ]
  ptypes[board[1, ]==0] <- 0
  bboard <- as.numeric(eye11[, ptypes + 1][-1, ])
  names(bboard) <- paste(rep(c("K", "R", "B", "P", "T", "k", "r", "b", "p", "t"), 12), 
                         rep(1:12, each = 10), sep = "")
  h1 <- as.numeric(ut3[, hand1 + 1][-1, ])
  h2 <- as.numeric(ut3[, hand2 + 1][-1, ])
  names(h1) <- paste(rep(c("K", "R", "B", "P"), each = 2), rep(c("x1", "x2")), sep = "")
  names(h2) <- paste(rep(c("K", "R", "B", "P"), each = 2), rep(c("x1", "x2")), sep = "")
  ans <- c(bboard, h1, h2)
  ## length(ans) ## 136
  ans
}


####
##  Position to prediction
####

ginds <- trinds
gind <- 1

makePosTables <- function(ginds) {
  senteChosen <- numeric()
  goteChosen <- numeric()
  senteMoves <- list()
  goteMoves <- list()
  sentePos <- numeric()
  gotePos <- numeric()
  for (gind in ginds) {
    game <- glist[[gind]]
    gs <- gstates[[gind]]
    mvs <- lmoves[[gind]]
    for (turn in 2:length(game)) {
      mv <- game[[turn]]
      prev <- gs[[turn - 1]]
      if (mv != "resign") {
        lmv <- mvs[[turn]]
        if (turn %% 2 == 1) {
          senteChosen <- c(senteChosen, which(allmoves==mv)[1])
          senteMoves <- c(senteMoves, list(match(lmv, allmoves)))
          sentePos <- rbind(sentePos, prev)
        } else {
          goteChosen <- c(goteChosen, which(allmoves==mv)[1])
          goteMoves <- c(goteMoves, list(match(lmv, allmoves)))
          gotePos <- rbind(gotePos, prev)
        }
      }
    }
  }
  senteX <- t(apply(sentePos, 1, expand_state))
  rownames(senteX) <- NULL
  goteX <- t(apply(gotePos, 1, expand_state))
  rownames(goteX) <- NULL
  senteChosen <- factor(senteChosen, 1:nmoves)
  goteChosen <- factor(goteChosen, 1:nmoves)
  list(senteX = senteX, senteChosen = senteChosen,
       senteMoves = senteMoves,
       goteX = goteX, goteChosen = goteChosen,
       goteMoves = goteMoves)
}

resTr <- makePosTables(trinds)
resTe <- makePosTables(teinds)

trfit <- glmnet(resTr$senteX, resTr$senteChosen, family = "multinomial")
