source("doubutsu1/gen0/sourceP.R")
source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/viz.R")
Bs <- readRDS("doubutsu1/prediction/spfit1_sente.rds")
Bg <- readRDS("doubutsu1/prediction/spfit1_gote.rds")
source("doubutsu1/gen0/sourceE.R")
mult <- 2
bs <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bg <- readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
# bs <- mult * readRDS("doubutsu1/gen0/temp_evS.rds")
# bg <- mult * readRDS("doubutsu1/gen0/temp_evG.rds")
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

ai_move2 <- ai_moveP
Bs2 <- Bs
Bg2 <- Bg
ai_move1 <- ai_moveE
Bs1 <- bs
Bg1 <- bg

winners <- character()

i <- 1
while (i < 1001) {
  res <- selfplay(i, ai_moveE, bs, bg, nsample, mateXdepth)
  # if (i %% 2 ==0) {
  #   sente <- "Eval"; gote <- "Pol"
  #   res <- cvc(i, ai_moveE, bs, bg, ai_moveP, Bs, Bg, nsample, mateXdepth)
  # } else {
  #   sente <- "Pol"; gote <- "Eval"
  #   res <- cvc(i, ai_moveP, Bs, Bg, ai_moveE, bs, bg, nsample, mateXdepth)
  # }
  games[[i]] <- res
  draw_state(res$slist[[length(res$slist)]])
  if (length(res$mlist) %% 2 == 0) {
    winners[i] <- sente
    title("sente")
  } else {
    winners[i] <- gote
    title("gote")
  }
  
  if (i %% 100 == 0) {
    saveRDS(games, "doubutsu1/gen0/selfplaysE00.rds")
  }
  i <- i + 1
}
