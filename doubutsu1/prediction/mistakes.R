source("doubutsu1/source.R")
source("doubutsu1/viz.R")
Rcpp::sourceCpp("doubutsu1/Rsource.cpp")
Rcpp::sourceCpp("doubutsu1/Rsource2.cpp")
games <- readRDS("doubutsu1/lg_states.rds")
load("doubutsu1/lg_data.rda")
ginds <- which(gametable$len > 2)
alts <- readRDS("doubutsu1/solved_alts2.rds")
altu <- sapply(alts, function(v) uhash(v[1, ]))

games <- games[ginds]
gt <- gametable[ginds, ]
gl <- glist[ginds]
hashes <- lapply(games, function(v) sapply(v, uhash))


movechoice <- list()
for (i in 1:length(games)) {
  (res <- list(meta = gt[i, ]))
  nm <- length(games[[i]])
  game <- games[[i]]
  hs <- hashes[[i]]
  (mvs <- gl[[i]])
  Schoice <- list()
  Gchoice <- list()
  for (j in 2:nm) {
    h <- hs[j-1]
    stopifnot(h %in% altu)
    alt <- alts[[which(altu==h)[1]]]
    mn <- alt[-1, 1:2]
    if (j %% 2==0) mn[, 1] <- -mn[, 1]
    chi <- which(rownames(mn)==mvs[j])
    mn2 <- mn[-chi, , drop = FALSE]
    mn2 <- mn2[order(-mn2[, 1]/(mn2[, 2]+1)), , drop = FALSE]
    mn <- rbind(mn[chi, , drop = FALSE], mn2)
    # if (nrow(mn2)==1) {
    #   v <- rownames(alt[-1, ])
    #   rownames(mn) <- c(mvs[j], setdiff(v, mvs[j]))
    # }
    if (j %% 2 == 1) {
      Schoice <- c(Schoice, list(mn))
    } else {
      Gchoice <- c(Gchoice, list(mn))
    }
  }
  res$Schoice <- Schoice
  res$Gchoice <- Gchoice
}


saveRDS(movechoice, "doubutsu1/lg_movechoice.rds")
