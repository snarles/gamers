source("doubutsu1/gen0/sourceP.R")
source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/viz.R")
Bs <- readRDS("doubutsu1/prediction/spfit1_sente.rds")
Bg <- readRDS("doubutsu1/prediction/spfit1_gote.rds")
source("doubutsu1/gen0/sourceE.R")
bs <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bg <- readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
nsample = 1; mateXdepth = 5
games <- list()




####

seed <- 0
ai_move1 <- ai_moveP
Bs1 <- Bs
Bg1 <- Bg
ai_move2 <- ai_moveE
Bs2 <- bs
Bg2 <- bg

winners <- character()

i <- 1
while (i < 101) {
  #res <- selfplay(i, ai_moveP, Bs, Bg, nsample, mateXdepth)
  if (i %% 2 ==0) {
    res <- cvc(i, ai_moveE, bs, bg, ai_moveP, Bs, Bg, nsample, mateXdepth)
    sente <- "Eval"; gote <- "Pol"
  } else {
    res <- cvc(i, ai_moveP, Bs, Bg, ai_moveE, bs, bg, nsample, mateXdepth)
    sente <- "Pol"; gote <- "Eval"
  }
  games[[i]] <- res
  draw_state(res$slist[[length(res$slist)]])
  if (length(res$mlist) %% 2 == 0) {
    winners[i] <- sente
    title(sente)
  } else {
    winners[i] <- gote
    title(gote)
  }
  
  if (i %% 100 == 0) {
    saveRDS(games, "doubutsu1/gen0/selfplaysPvE.rds")
  }
  i <- i + 1
}
