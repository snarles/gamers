source("doubutsu1/gen0/selfplay.R")
source("doubutsu1/gen0/corrective.R")
source("doubutsu1/viz.R")
source("doubutsu1/gen0/sourceE.R")
bs <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
bg <- readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
nsample = 1; mateXdepth = 5
lessons <- list()
move.limit <- 10

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
statesS <- states[states[, 4] %% 2 == 1, ]
statesG <- states[states[, 4] %% 2 == 0, ]
hashesS <- apply(statesS, 1, hash_state)
hashesG <- apply(statesG, 1, hash_state)
tabS <- table(hashesS)
tabG <- table(hashesG)
set.seed(0)
pS <- invP(tabS, -0.8)
pG <- invP(tabG, -0.8)
indsS <- sample(length(hashesS), 5000, FALSE, prob = pS[hashesS])
indsG <- sample(length(hashesG), 5000, FALSE, prob = pG[hashesG])
states <- rbind(statesS[indsS, ], statesG[indsG, ])
####

winners <- character()

get_lesson <- function(i) {
  start <- states[i, ]
  if (!is.na(mateX(start, 3))) return(NULL)
  analyze_state(start, bs, bg, nsample, mateXdepth, nreps = 4, move.limit, TRUE)
}

i <- (floor(length(lessons)/100)+1) * 100 + 1
while (i < nrow(states) + 1) {
  lessons[[i]] <- get_lesson(i)
  if (i %% 100 == 0) {
    saveRDS(lessons, "doubutsu1/gen0/lessons01.rds")
  }
  i <- i + 1
}
