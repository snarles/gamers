####
##  More of a setup script at this point
####

source("doubutsu1/source.R")
# sourceCpp("doubutsu1/Rsource2.cpp")
hashtab <- readRDS("doubutsu1/hashtab.rds")
matein <- readRDS("doubutsu1/matein.rds")

games <- readRDS("doubutsu1/lg_states.rds")
hashes <- lapply(games, function(v) sapply(v, hash_state))
saveRDS(hashes, "doubutsu1/lg_hashes.rds")

load("doubutsu1/lg_data.rda", verbose = TRUE)
length(games)
length(glist)
