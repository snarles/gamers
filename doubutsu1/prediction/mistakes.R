source("doubutsu1/source.R")
source("doubutsu1/viz.R")
Rcpp::sourceCpp("doubutsu1/Rsource.cpp")
Rcpp::sourceCpp("doubutsu1/Rsource2.cpp")
games <- readRDS("doubutsu1/lg_states.rds")
load("doubutsu1/lg_data.rda")
ginds <- which(gametable$len > 2)
alts <- readRDS("doubutsu1/solved_alts2.rds")
source("doubutsu1/sourceJP.R")
alt0 <- solve_state_raw(init_state, TRUE)
res0 <- analysis_to_values(alt0)
alts <- c(alts, res0)
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
  movechoice <- c(movechoice, list(res))
}

source("doubutsu1/prediction/mistake_source.R")
# saveRDS(movechoice, "doubutsu1/lg_movechoice.rds")
movechoice <- readRDS("doubutsu1/lg_movechoice.rds")

Schoices <- do.call(c, lapply(movechoice, `[[`, "Schoice"))
Gchoices <- do.call(c, lapply(movechoice, `[[`, "Gchoice"))


mats <- Schoices
xxs <- lapply(mats, expand_mn)
sort(sapply(xxs, ncol))
i <- 1



mnS <- mn_sgd(Schoices)
for (i in 1:20) {
#  mnS <- smooth_mn(mnS)
  mnS <- mn_sgd(Schoices, mnS, l1p=1e-3, l2p = 0, eps = 0.1)
  print(mn_loss(Schoices, mnS)[1:2])
}

plot(mnS, type = "l")


mnG <- mn_sgd(Gchoices)
for (i in 1:20) {
  #  mnG <- smooth_mn(mnG)
  mnG <- mn_sgd(Gchoices, mnG, l1p=1e-3, l2p = 0, eps = 0.1)
  print(mn_loss(Gchoices, mnG)[1:2])
}

plot(mnG, type = "l")
plot(mnG, type = "l"); lines(mnS, col = "red")

names(mnS) <- c("win", "lose", 
                paste0("w", 1:MN_MAX_LEN),
                paste0("l", 1:MN_MAX_LEN))
names(mnG) <- names(mnS)

# save(mnS, mnG, file = "doubutsu1/prediction/mn00.rds")

####
##  Blundergraph
####
h0 <- uhash(init_state)
movegraph <- list()
for (i in 1:length(games)) {
  (res <- list(meta = gt[i, ]))
  vals <- numeric()
  nm <- length(games[[i]])
  game <- games[[i]]
  hs <- hashes[[i]]
  (mvs <- gl[[i]])
  choices <- list()
  for (j in 1:nm) {
    h <- h0
    if (j > 1) h <- hs[j-1]
    stopifnot(h %in% altu)
    alt <- alts[[which(altu==h)[1]]]
    vals[j] = alt[1, 1]
    mn <- alt[-1, 1:2]
    chi <- which(rownames(mn)==mvs[j])
    mn2 <- mn[-chi, , drop = FALSE]
    mn2 <- mn2[order(-mn2[, 1]/(mn2[, 2]+1)), , drop = FALSE]
    mn <- rbind(mn[chi, , drop = FALSE], mn2)
    choices[[j]]=mn
  }
  res$choices <- choices
  res$vals <- vals
  movegraph <- c(movegraph, list(res))
}

# saveRDS(movegraph, "doubutsu1/prediction/movegraphs.rds")
movegraph <- readRDS("doubutsu1/prediction/movegraphs.rds")

np <- 10
plot(NA, NA, xlim = c(0, 50), ylim = c(1, 3 * np + 2), ylab = "", xlab = "turn")
inds <- sample(length(movegraph), np)
for (i in 1:np) {
  abline(3 * i, 0, lty = 3)
  lines(1:length(movegraph[[inds[i]]]$vals),
        3 * i + movegraph[[inds[i]]]$vals, col = rainbow(length(movegraph))[inds[i]])
}