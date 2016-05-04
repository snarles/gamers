source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/viz.R")
source("doubutsu1/gen0/sourceE.R")
bs <- 0.11 * readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
#bg <- 0 * readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
bg <- readRDS("doubutsu1/gen0/adapt0/bg1_01_769.rds")
Bs <- bs
Bg <- bg
nsample = 1; mateXdepth = 3
games <- list()

newsente <- FALSE

####

winners <- character()
eps <- 0.02


for (ii in 1:1000) {
  # option 1
  if (newsente) {
    res <- selfplay(ii, ai_moveE, Bs, bg, nsample, mateXdepth)
    sente <- "new"; gote <- "old"
  } else {
    res <- selfplay(ii, ai_moveE, bs, Bg, nsample, mateXdepth)
    sente <- "old"; gote <- "new"
  }
  
  games <- c(games, list(res))
  draw_state(res$slist[[length(res$slist)]])
  if (length(res$mlist) %% 2 == 0) {
    winners <- c(winners, sente)
    title(sente, sub = sum(winners == sente)/length(winners))
  } else {
    winners <- c(winners, gote)
    title(gote, sub = sum(winners == gote)/length(winners))
  }
  
  states <- do.call(rbind, res$slist)
  gamerec <- res$mlist
  if (gote == "new") {
    
    if (rev(winners)[1] == "new") {
      sgn <- 1
    } else {
      sgn <- -1
    }
    
    gchoice_inds <- (1:floor(length(res$mlist)/2))*2
    for (i in gchoice_inds) {
      alts <- build_tree(states[i -1, ], 1, 200)
      mvs <- legal_moves(states[i-1, ], alts)
      alts <- alts[-1, , drop = FALSE]
      X <- t(apply(alts, 1, expandState))
      ps <- mcm_probs(X, Bg)
      if (sgn == 1) {
        ic <- match(gamerec[i], mvs)
        ps[ic] <- ps[ic] - 1
      } else {
        if (gamerec[i] %in% mvs) {
          ps[match(gamerec[i], mvs)] <- ps[match(gamerec[i], mvs)] + 1
          ps <- ps - 1/length(ps)
        }
        else {
          ps <- 0 * ps
        }
      }
      Bg <- gradient2(Bg, X,  -eps * ps)
      Bg <- shrinker(Bg, eps * 0.001, eps * 0.001)
    }
  }
  
  print(table(winners))
  
}
plot(cumsum(winners == "new"), type = "l")
# saveRDS(Bg, "doubutsu1/gen0/adapt0/bg1_01_769.rds")