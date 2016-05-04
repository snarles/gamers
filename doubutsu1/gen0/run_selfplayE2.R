
source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/viz.R")

source("doubutsu1/gen0/sourceE2.R")
mult <- 1
bs <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bg <- readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
bse <- mult * readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bge <- mult * readRDS("doubutsu1/gen0/n01_evG.rds")
nsample = 5; mateXdepth = 5; expo <- 2
#games <- list()
#games <- readRDS("doubutsu1/gen0/selfplaysE00.rds")



####

winners <- character()

i <- length(games) + 1
while (i < 2001) {
  res <- selfplay2(i, ai_moveE2, bs, bg, bse, bge, nsample, mateXdepth, expo)
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
    saveRDS(games, "doubutsu1/gen0/selfplaysE02.rds")
  }
  i <- i + 1
}
