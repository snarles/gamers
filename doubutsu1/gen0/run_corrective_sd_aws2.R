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
hashes <- apply(states, 1, hash_state)
uninds <- match(unique(hashes), hashes)
states <- states[uninds, ]






print_state_JP(states[1, ])
