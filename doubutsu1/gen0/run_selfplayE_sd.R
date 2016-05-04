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
# bg <- readRDS("doubutsu1/gen0/lg01_evG.rds")
nsample = 3; mateXdepth = 5
games <- list()
#games <- readRDS("doubutsu1/gen0/selfplaysE00.rds")

sente <- "old"; gote <- "og"


states <- readRDS("doubutsu1/lg_states.rds")
states <- states[sapply(states, length) > 2]
states <- lapply(states, do.call, what = rbind)
states <- do.call(rbind, states)

####

winners <- character()

i <- length(games) + 1
while (i < 2001) {
  set.seed(i)
  flag <- TRUE
  while(flag) {
    start <- states[sample(nrow(states), 1), ]
    if (is.na(mateX(start, 3))) flag <- FALSE    
  }
  res <- selfplay(i, ai_moveE, bs, bg, nsample, mateXdepth, start)
  # if (i %% 2 ==0) {
  #   sente <- "Eval"; gote <- "Pol"
  #   res <- cvc(i, ai_moveE, bs, bg, ai_moveP, Bs, Bg, nsample, mateXdepth)
  # } else {
  #   sente <- "Pol"; gote <- "Eval"
  #   res <- cvc(i, ai_moveP, Bs, Bg, ai_moveE, bs, bg, nsample, mateXdepth)
  # }
  games[[i]] <- res
  #draw_state(res$slist[[length(res$slist)]])
  if (res$winner == "sente") {
    winners[i] <- sente
    #title(sente, sub = sum(winners == sente)/length(winners))
  } else {
    winners[i] <- gote
    #title(gote, sub = sum(winners == gote)/length(winners))
  }
  
  if (i %% 100 == 0) {
    draw_state(res$slist[[length(res$slist)]])
    title(sum(winners == sente)/length(winners))
    saveRDS(games, "doubutsu1/gen0/selfplaysES00.rds")
  }
  i <- i + 1
}
