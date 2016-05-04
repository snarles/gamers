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
move.limit <- 3
#games <- readRDS("doubutsu1/gen0/selfplaysE00.rds")

sente <- "old"; gote <- "og"


invP <- function(v, expo = -1) {
  v <- v^expo
  v/sum(v)
}

states <- readRDS("doubutsu1/lg_states.rds")
states <- states[sapply(states, length) > 2]
states <- lapply(states, do.call, what = rbind)
states <- do.call(rbind, states)
length(unique(apply(states, 1, hash_state)))
set.seed(0)
states <- states[sample(nrow(states)), ]

####

winners <- character()

i <- (floor(length(games)/99)+1) * 100 + 1
while (i < nrow(states) + 1) {
  set.seed(i)
  flag <- TRUE
  while(flag) {
    #start <- states[sample(nrow(states), 1), ]
    start <- states[i, ]
    if (is.na(mateX(start, 3))) {
      flag <- FALSE
    } else {
      i <- i + 1
    }

  }
  res <- selfplay(i, ai_moveE, bs, bg, nsample, mateXdepth, start, move.limit)
  # if (i %% 2 ==0) {
  #   sente <- "Eval"; gote <- "Pol"
  #   res <- cvc(i, ai_moveE, bs, bg, ai_moveP, Bs, Bg, nsample, mateXdepth)
  # } else {
  #   sente <- "Pol"; gote <- "Eval"
  #   res <- cvc(i, ai_moveP, Bs, Bg, ai_moveE, bs, bg, nsample, mateXdepth)
  # }
  games <- c(games, list(res))
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
    #title(sum(winners == sente)/length(winners))
    title(i)
    saveRDS(games, "doubutsu1/gen0/selfplaysES01.rds")
  }
  i <- i + 1
}
