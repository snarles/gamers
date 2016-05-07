####
##  More of a setup script at this point
####

source("doubutsu1/source.R")
Rcpp::sourceCpp("doubutsu1/Rsource.cpp")
# sourceCpp("doubutsu1/Rsource2.cpp")
hashtab <- readRDS("doubutsu1/hashtab.rds")
matein <- readRDS("doubutsu1/matein.rds")

games <- readRDS("doubutsu1/lg_states.rds")
hashes <- readRDS("doubutsu1/lg_hashes.rds")

load("doubutsu1/lg_data.rda", verbose = TRUE)
length(games)
length(glist)

allmoves <- readRDS("doubutsu1/allmoves.rds")

ginds <- which(sapply(glist, length) > 2)
set.seed(0)
trinds <- sort(sample(ginds, 500, FALSE))
teinds <- setdiff(ginds, trinds)

alts <- readRDS("doubutsu1/altMoves.rds")
lmoves <- readRDS("doubutsu1/lmoves.rds")
gstates <- readRDS("doubutsu1/lg_states.rds")

makeTable <- function(ginds) {
  senteAlts <- list()
  goteAlts <- list()
  senteChoice <- numeric()
  goteChoice <- numeric()
  sentePl <- character()
  gotePl <- character()
  senteChosen <- character()
  goteChosen <- character()
  senteMoves <- list()
  goteMoves <- list()
  sentePos <- numeric()
  gotePos <- numeric()
  senteTurn <- numeric()
  goteTurn <- numeric()
  for (gind in ginds) {
    ALT <- alts[[gind]]
    meta <- gametable[gind, ]
    game <- glist[[gind]]
    gs <- gstates[[gind]]
    mvs <- lmoves[[gind]]
    for (turn in 2:length(ALT)) {
      alt <- ALT[[turn]]
      choice <- which(alt[, 1]==1)[1]
      mat <- t(apply(alt, 1, expandState))
      mv <- game[[turn]]
      prev <- gs[[turn - 1]]
      lmv <- mvs[[turn]]
      if (turn %% 2 == 1) {
        senteAlts <- c(senteAlts, list(mat))
        senteChoice <- c(senteChoice, choice)
        sentePl <- c(sentePl, meta$sente)
        senteChosen <- c(senteChosen, mv)
        senteMoves <- c(senteMoves, list(lmv))
        sentePos <- rbind(sentePos, prev)
        senteTurn <- rbind(senteTurn, turn)
      } else {
        goteAlts <- c(goteAlts, list(mat))
        goteChoice <- c(goteChoice, choice)
        gotePl <- c(gotePl, meta$gote)
        goteChosen <- c(goteChosen, mv)
        goteMoves <- c(goteMoves, list(lmv))
        gotePos <- rbind(gotePos, prev)
        goteTurn <- rbind(goteTurn, turn)
      }
    }
  }
  senteX <- t(apply(sentePos, 1, expandState))
  rownames(senteX) <- NULL
  goteX <- t(apply(gotePos, 1, expandState))
  rownames(goteX) <- NULL
  list(senteAlts = senteAlts, senteChoice = senteChoice,
       goteAlts = goteAlts, goteChoice = goteChoice,
       sentePl = sentePl, gotePl = gotePl,
       senteX = senteX, senteChosen = senteChosen,
       senteMoves = senteMoves,
       goteX = goteX, goteChosen = goteChosen,
       goteMoves = goteMoves, senteTurn = senteTurn, goteTurn = goteTurn)
}

resTr <- makeTable(trinds)
resTe <- makeTable(teinds)
