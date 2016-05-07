
source("doubutsu1/source.R")
source("doubutsu1/viz.R")
Rcpp::sourceCpp("doubutsu1/Rsource.cpp")
Rcpp::sourceCpp("doubutsu1/Rsource2.cpp")
source("doubutsu1/sourceJP.R")
# load("doubutsu1/lg_data.rda")
# lg_states <- readRDS("doubutsu1/lg_states.rds")
# ginds <- which(gametable$len > 2)


uhash <- function(state) paste0(state[4] %%2, paste(state[5:48], collapse = ""))

# alts <- readRDS("doubutsu1/solved_alts.rds")
# alts <- do.call(c, alts)
# s1 <- sapply(alts, function(v) uhash(v[1, ]))
# alts <- alts[match(unique(s1), s1)]
# saveRDS(alts, "doubutsu1/solved_alts2.rds")
alts <- readRDS("doubutsu1/solved_alts2.rds")
uh <- sapply(alts, function(v) uhash(v[1, ]))
hashX <- sapply(alts, function(v) hash_state(v[1, ]))
mateXX <- sapply(alts, function(v) {
  mX <- mateX(v[1, ], 1)
  if (!is.na(mX) && mX > 0) return(0)
  inX <- v[-1, 2]
  vals <- v[-1, 1]
  if ((v[1, 4]==1 && v[1, 1]==-1) || (v[1, 4]==0 && v[1, 1]==1)) {
    return(min(inX[vals==v[1, 1]]))
  }
  NA
})
zinds <- which(mateXX == Inf)
i <- sample(which(mateXX==5), 1)
draw_state(alts[[i]][1, ], title = TRUE)
v <- alts[[i]]
SOLVER_DIR <- "~/Downloads/dobutsu"
ares <- solve_state_raw(alts[[i]][1, ], TRUE)
state <- alts[[i]][1, ]


hashes <- readRDS("doubutsu1/lg_hashes.rds")
ah <- do.call(c, hashes)
filt <- hashX %in% ah
matein <- list()
matein[hashX[filt]] <- mateXX[filt]
length(matein)

saveRDS(matein, "doubutsu1/matein_solver.rds")

setwd("~/github/gamers")

