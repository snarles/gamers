source("doubutsu1/lg_analysis_setup.R")
lmoves <- readRDS("doubutsu1/lmoves.rds")

####
##  Markov chain model
####

makeMarkovTables <- function(ginds) {
  senteTab <- matrix("", 0, 2)
  goteTab <- matrix("", 0, 2)
  senteMoves <- list()
  goteMoves <- list()
  colnames(senteTab) <- c("prev", "next")
  colnames(goteTab) <- c("prev", "next")
  for (gind in ginds) {
    game <- glist[[gind]]
    mvs <- lmoves[[gind]]
    for (turn in 2:length(game)) {
      mv <- game[turn]
      prev <- game[turn - 1]
      if (mv != "resign") {
        lmv <- mvs[[turn]]
        if (turn %% 2 == 1) {
          senteTab <- rbind(senteTab, c(prev, mv))
          senteMoves <- c(senteMoves, list(lmv))
        } else {
          goteTab <- rbind(goteTab, c(prev, mv))
          goteMoves <- c(goteMoves, list(lmv))
        }
      }
    }
  }
  list(senteTab = senteTab, goteTab = goteTab,
       senteChoice = senteMoves, goteChoice = goteMoves)
}

aggPreds <- function(tab, xs) {
  ans <- list()
  for (xx in xs) {
    if (!xx %in% tab[, 1]) {
      ans[[xx]] <- c(unknown = 1)      
    } else {
      m <- table(tab[tab[, 1]==xx, 2])
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


# for (i in 1:length(senteTrue)) {
#   if (!senteTrue[i] %in% resTe$senteChoice[[i]]) print(i)
# }
# for (i in 1:length(goteTrue)) {
#   if (!goteTrue[i] %in% resTe$goteChoice[[i]]) print(i)
# }


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
for (i in 1:length(goteTrue)) {
  v <- gotePrevTe[i]
  goteTrue[i]
  rule <- goteRule[[v]]
  (gotePred[i] <- names(rule)[rule == max(rule)][1])
  if (goteTrue[i] %in% names(rule)) {
    goteProbs[i] <- rule[names(rule)==goteTrue[i]]
  }
}


sum(sentePred == senteTrue)/length(senteTrue) # 0.2668428
sum(gotePred == goteTrue)/length(goteTrue) # 0.3180109

sum(senteProb == 0)/length(senteTrue) # 0.1902246
sum(goteProb == 0)/length(senteTrue) # 0.1875826
hist(senteProbs)
hist(goteProbs)


####
##  Limit to legal moves
####


senteProbs <- numeric()
sentePred <- character()
for (i in 1:length(senteTrue)) {
  v <- sentePrevTe[i]
  senteTrue[i]
  rule <- senteRule[[v]]
  lg <- resTe$senteChoice[[i]]
  rule <- rule[names(rule) %in% lg]
  if (length(rule) == 0) {
    sentePred[i] <- "unknown"
    senteProbs[i] <- 0
  } else {
    rule <- rule/sum(rule)
    (sentePred[i] <- names(rule)[rule == max(rule)][1])
    if (senteTrue[i] %in% names(rule)) {
      senteProbs[i] <- rule[names(rule)==senteTrue[i]]
    }    
  }
}

goteProbs <- numeric()
gotePred <- character()
for (i in 1:length(goteTrue)) {
  v <- gotePrevTe[i]
  goteTrue[i]
  rule <- goteRule[[v]]
  lg <- resTe$goteChoice[[i]]
  rule <- rule[names(rule) %in% lg]
  if (length(rule) == 0) {
    gotePred[i] <- "unknown"
    goteProbs[i] <- 0
  } else {
    rule <- rule/sum(rule)
    (gotePred[i] <- names(rule)[rule == max(rule)][1])
    if (goteTrue[i] %in% names(rule)) {
      goteProbs[i] <- rule[names(rule)==goteTrue[i]]
    }    
  }
}

sum(sentePred == senteTrue)/length(senteTrue) # 0.440775
sum(gotePred == goteTrue)/length(goteTrue) # 0.4588383

sum(senteProb == 0)/length(senteTrue) # 0.1902246
sum(goteProb == 0)/length(senteTrue) # 0.1875826
hist(senteProbs)
hist(goteProbs)