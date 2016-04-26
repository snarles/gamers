source("doubutsu1/lg_analysis_setup.R")

####
##  Markov chain model
####

makeMarkovTables <- function(ginds) {
  senteTab <- matrix("", 0, 2)
  goteTab <- matrix("", 0, 2)
  colnames(senteTab) <- c("prev", "next")
  colnames(goteTab) <- c("prev", "next")
  for (gind in ginds) {
    game <- glist[[gind]]
    for (turn in 2:length(game)) {
      mv <- game[turn]
      prev <- game[turn - 1]
      if (mv != "resign") {
        if (turn %% 2 == 1) {
          senteTab <- rbind(senteTab, c(prev, mv))
        } else {
          goteTab <- rbind(goteTab, c(prev, mv))
        }
      }
    }
  }
  list(senteTab = senteTab, goteTab = goteTab)
}

aggPreds <- function(tab, xs) {
  ans <- list()
  for (xx in xs) {
    if (!xx %in% tab[, 1]) {
      ans[[xx]] <- c(unknown = 1)      
    } else {
      m <- table(tab[tab[, 1]==xx, ])
      ans[[xx]] <- m/sum(m)
    }
  }
  ans
}

resTr <- makeMarkovTables(trinds)
resTe <- makeMarkovTables(teinds)

## predict the test stuff

sentePrevTe <- resTe$senteTab[, 1]
gotePrevTe <- resTe$goteTab[, 1]

senteTrue <- resTe$senteTab[, 2]
goteTrue <- resTe$goteTab[, 2]

senteRule <- aggPreds(resTr$senteTab, unique(sentePrevTe))
goteRule <- aggPreds(resTr$goteTab, unique(gotePrevTe))


senteProbs <- numeric()
sentePred <- character()
for (i in 1:length(senteTrue)) {
  v <- sentePrevTe[i]
  senteTrue[i]
  rule <- senteRule[[v]]
  (sentePred[i] <- names(rule)[rule == max(rule)][1])
  if (senteTrue[i] %in% names(rule)) {
    senteProbs[i] <- rule[names(rule)==senteTrue[i]]
  }
}

goteProbs <- numeric()
gotePred <- character()
for (i in 1:length(senteTrue)) {
  v <- gotePrevTe[i]
  goteTrue[i]
  rule <- goteRule[[v]]
  (gotePred[i] <- names(rule)[rule == max(rule)][1])
  if (goteTrue[i] %in% names(rule)) {
    goteProbs[i] <- rule[names(rule)==goteTrue[i]]
  }
}

