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
states1 <- rbind(statesS[indsS, ], statesG[indsG, ])


games <- c(readRDS("doubutsu1/gen0/aws_sp_06000.rds"),
           readRDS("doubutsu1/gen0/aws_sp_12000.rds"),
           readRDS("doubutsu1/gen0/aws_sp_18000.rds"))
games <- games[!sapply(games, is.null)]
games <- lapply(games, function(v) {
  mat <- do.call(rbind, v$slist)
  if (length(v$mlist) %% 2 == 0) {
    # sente win
    mat[mat[, 4] %% 2 == 0, 1] <- 0
    mat[mat[, 4] %% 2 == 1, 1] <- 1
  } else {
    mat[mat[, 4] %% 2 == 0, 1] <- 1
    mat[mat[, 4] %% 2 == 1, 1] <- 0
  }
  mat
})
games <- do.call(rbind, games)
gc()
statesS <- games[games[, 4] %% 2 == 1, ]
statesG <- games[games[, 4] %% 2 == 0, ]
hashesS <- apply(statesS, 1, hash_state)
hashesG <- apply(statesG, 1, hash_state)
length(unique(hashesS))
tabS <- table(hashesS)
tabG <- table(hashesG)
pS <- invP(tabS, -0.5)
pG <- invP(tabG, -0.5)
indsS <- sample(length(hashesS), 20000, FALSE, prob = pS[hashesS])
indsG <- sample(length(hashesG), 20000, FALSE, prob = pG[hashesG])
states2 <- rbind(statesS[indsS, ], statesG[indsG, ])
states <- rbind(states1, states2)
rm(list = c("states1", "states2", "statesS", "statesG"))
gc()
nrow(states) # 50000
length(unique(apply(states, 1, hash_state))) # 40175
####

winners <- character()

get_lesson <- function(i) {
  start <- states[i, ]
  if (!is.na(mateX(start, 3))) return(NULL)
  analyze_state(start, bs, bg, nsample, mateXdepth, nreps = 4, move.limit, TRUE)
}

# sink("temp.txt")
# lesson <- get_lesson(919)
# sink()

library(parallel)
t1 <- proc.time()
RES <- mclapply(1:100, get_lesson, mc.preschedule = FALSE, mc.cores = 8)
proc.time() - t1
