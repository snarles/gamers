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
states0 <- do.call(rbind, states)
hashes <- apply(states0, 1, function(state) paste0(state[4] %%2, paste(state[5:48], collapse = "")))
uninds <- match(unique(hashes), hashes)
states <- states0[uninds, ]

saveRDS(states, "doubutsu1/lg_states_mat.rds")
saveRDS(hashes, "doubutsu1/uhashes.rds")
saveRDS(states, "doubutsu1/ustates_lg.rds")
source("doubutsu1/sourceJP.R")


ff <- solve_state_raw(states[300, ], TRUE)

odir0 <- getwd()

t1 <- proc.time()
res <- list()
for (i in 1:nrow(states)) {
  ff <- solve_state_raw(states[i, ])
  res <- c(res, list(ff))
}
proc.time() - t1
saveRDS(res, "doubutsu1/solver_analyses_raw.rds")

setwd(odir0)

analyses <- res
length(res)

## Make states to train util function



v <- analyses[[2120]]
sink("temp.txt"); cats(v); sink()
res <- analysis_to_values(v)
res[[1]][, 1:5]
draw_state(res[[1]][1, ])
print_state(res[[1]][1, ])

