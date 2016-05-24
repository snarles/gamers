source("doubutsu1/source.R")
Rcpp::sourceCpp("doubutsu1/Rsource.cpp")
source("doubutsu1/sourceJP.R")
source("doubutsu1/viz.R")
source("doubutsu1/matein_based_AI.R")

nct <- 76
res <- list()

# while (nct > 70) {
  gms <- character()
  flag <- TRUE
  while(flag) {
    gm <- get_sp(seek_val = nct, move.limit = 30)
    ll <- length(gm)
    gm <- paste(gm, collapse = " ")
    if (gm %in% gms) flag <- FALSE
    gms <- c(gms, gm)
  }
  res[[nct]] <- gms
  nct <- nct - 1  
  length(gms)
# }

  
resigned <- grep("resign", gms)
gmsub <- gms[resigned]
rct <- sapply(gmsub, function(v) which(strsplit(v, " ")[[1]] == "resign"))
table(rct %% 2)
