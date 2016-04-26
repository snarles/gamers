####
##  Extract some more features
####

source("doubutsu1/source.R")
sourceCpp("doubutsu1/Rsource2.cpp")
hashtab <- readRDS("doubutsu1/hashtab.rds")
matein <- readRDS("doubutsu1/matein.rds")

games <- readRDS("doubutsu1/lg_states.rds")
hashes <- lapply(games, function(v) sapply(v, hash_state))
saveRDS(hashes, "doubutsu1/lg_hashes.rds")

load("doubutsu1/lg_data.rda", verbose = TRUE)
length(games)
length(glist)

save()

ginds <- which(sapply(glist, length) > 2)
(gno <- sample(ginds, 1))
(game <- glist[[gno]])
mi <- matein[hashes[[gno]]]
names(mi) <- NULL
if (rev(game)[1] == "resign") mi <- c(mi, Inf)
cbind(game, mi)

gametable[gno, ]


####
##  Player stats?
####

sum(gametable$sente == gametable$loser)
sum(gametable$sente == gametable$winner)
