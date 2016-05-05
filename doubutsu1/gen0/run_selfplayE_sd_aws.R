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


states <- readRDS("doubutsu1/lg_states.rds")
states <- states[sapply(states, length) > 2]
states <- lapply(states, do.call, what = rbind)
states <- do.call(rbind, states)
length(unique(apply(states, 1, hash_state)))
set.seed(0)
states <- states[sample(nrow(states)), ]
states <- rbind(states, states[sample(nrow(states), 1000), ])

nrow(states) # 18080

####

get_sp <- function(i) {
  start <- states[i, ]
  if (!is.na(mateX(start, 3))) return(NULL)
  selfplay(i, ai_moveE, bs, bg, sample(5, 1), mateXdepth, start, 10)
}

library(parallel)

t1 <- proc.time()
mcres <- mclapply(1:6000, get_sp, mc.cores = 8)
proc.time() - t1
saveRDS(mcres, paste0("doubutsu1/gen0/aws_sp_06000.rds"))

t1 <- proc.time()
mcres <- mclapply(6001:12000, get_sp, mc.cores = 8)
proc.time() - t1
saveRDS(mcres, paste0("doubutsu1/gen0/aws_sp_12000.rds"))

t1 <- proc.time()
mcres <- mclapply(12001:18000, get_sp, mc.cores = 8)
proc.time() - t1
saveRDS(mcres, paste0("doubutsu1/gen0/aws_sp_18000.rds"))

