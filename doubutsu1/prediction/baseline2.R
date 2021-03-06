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
  senteChosen <- character()
  goteChosen <- character()
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
          senteChosen <- c(senteChosen, mv)
          senteMoves <- c(senteMoves, list(lmv))
          sentePos <- rbind(sentePos, prev)
        } else {
          goteChosen <- c(goteChosen, mv)
          goteMoves <- c(goteMoves, list(lmv))
          gotePos <- rbind(gotePos, prev)
        }
      }
    }
  }
  senteX <- t(apply(sentePos, 1, expand_state))
  rownames(senteX) <- NULL
  goteX <- t(apply(gotePos, 1, expand_state))
  rownames(goteX) <- NULL
  list(senteX = senteX, senteChosen = senteChosen,
       senteMoves = senteMoves,
       goteX = goteX, goteChosen = goteChosen,
       goteMoves = goteMoves)
}

filter_moves_by_count <- function(chosen, thres = 1) {
  tab <- table(chosen)
  smoves <- names(tab)[tab  > thres]
  chosen %in% smoves
}

legal_preds <- function(preds, legals) {
  mpreds <- character()
  for (i in 1:nrow(preds)) {
    inds <- intersect(legals[[i]], colnames(preds))
    if (length(inds) == 0) {
      mpreds[i] <- "unknown"
    } else {
      pvs <- preds[i, inds]
      mpreds[i] <- inds[pvs == max(pvs)][1]
    }
  }
  mpreds
}

resTr <- makePosTables(trinds)
resTe <- makePosTables(teinds)

## Sente predictions
filt <- filter_moves_by_count(resTr$senteChosen, 8)
trfitS <- glmnet(resTr$senteX[filt, ], resTr$senteChosen[filt], family = "multinomial", alpha = 1, maxit = 1e6)
preds <- predict(trfitS, resTe$senteX, s = 0.001)[,, 1]
nrow(resTe$senteX)
# head(preds)
prinds <- apply(preds, 1, function(v) which(v==max(v))[1])
mpreds <- colnames(preds)[prinds]
sum(mpreds == resTe$senteChosen)/length(mpreds) # 0.5627
mpreds <- legal_preds(preds, resTe$senteMoves)
sum(mpreds == resTe$senteChosen)/length(mpreds) # 0.5737

## Gote predictions
filt <- filter_moves_by_count(resTr$goteChosen, 8)
trfitG <- glmnet(resTr$goteX[filt, ], resTr$goteChosen[filt], family = "multinomial", alpha = 1, maxit=1e6)
preds <- predict(trfitG, resTe$goteX, s = 0.001)[,, 1]
prinds <- apply(preds, 1, function(v) which(v==max(v))[1])
mpreds <- colnames(preds)[prinds]
sum(mpreds == resTe$goteChosen)/length(mpreds) # 0.2962
mpreds <- legal_preds(preds, resTe$goteMoves)
sum(mpreds == resTe$goteChosen)/length(mpreds) # 0.3293
