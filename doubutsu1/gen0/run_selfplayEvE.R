source("doubutsu1/gen0/sourceP.R")
source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/viz.R")

source("doubutsu1/gen0/sourceE.R")
mult1 <- 1; mult2 <- 1
bs <- mult1 * readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bg <- mult1 * readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
Bs <- mult2 * readRDS("doubutsu1/gen0/n01_evS.rds")
Bg <- mult2 * readRDS("doubutsu1/gen0/n01_evG.rds")
nsample = 1; mateXdepth = 5
games <- list()




####

winners <- character()
sg <- character()

i <- 1
while (i < 21) {
  if (i %% 2 ==0) {
    sente <- "a1"; gote <- "a2"
    res <- cvc(i, ai_moveE, bs, bg, ai_moveE, Bs, Bg, nsample, mateXdepth)
  } else {
    sente <- "a2"; gote <- "a1"
    res <- cvc(i, ai_moveE, Bs, Bg, ai_moveE, bs, bg, nsample, mateXdepth)
  }
  games[[i]] <- res
  draw_state(res$slist[[length(res$slist)]])
  if (length(res$mlist) %% 2 == 0) {
    winners[i] <- sente
    sg[i] <- "sente"
    title(sente, sub = "sente")
  } else {
    winners[i] <- gote
    sg[i] <- "gote"
    title(gote, sub = "gote")
  }
  
  if (i %% 100 == 0) {
    #saveRDS(games, "doubutsu1/gen0/selfplaysE00.rds")
  }
  i <- i + 1
}

table(winners)