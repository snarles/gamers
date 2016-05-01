source("doubutsu1/prediction/spolicy.R")
source("doubutsu1/prediction/multiclassreg.R")

# library(parallel)

pY <- length(allmoves)
Y <- match(resTr$senteChosen, allmoves)
Yc <- lapply(resTr$senteMoves, function(v) match(v, allmoves))
X <- resTr$senteX
Xte <- resTe$senteX
Yte <- match(resTe$senteChosen, allmoves)
Ycte <- lapply(resTe$senteMoves, function(v) match(v, allmoves))
B <- matrix(0, ncol(X), pY)
B <- fitter(X, B, Y, l1p = 0.01, l2p = 0.02, eps = 0.005, nits = 100, verb = TRUE)
B <- fitter(X, B, Y, l1p = 1e-3, l2p = 1e-3, eps = 0.001, nits = 100, verb = TRUE)
B <- fitter(X, B, Y, l1p = 1e-3, l2p = 1e-3, eps = 0.001, nits = 100, verb = TRUE)
predict_acc(X, B, Y, Yc)[1]
res_sp_sente <- predict_acc(Xte, B, Yte, Ycte)
res_sp_sente[1] # 0.563
# saveRDS(B, "doubutsu1/prediction/spfit1_sente.rds")

## GOTE

Y <- match(resTr$goteChosen, allmoves)
Yc <- lapply(resTr$goteMoves, function(v) match(v, allmoves))
X <- resTr$goteX
Xte <- resTe$goteX
Yte <- match(resTe$goteChosen, allmoves)
Ycte <- lapply(resTe$goteMoves, function(v) match(v, allmoves))
B <- matrix(0, ncol(X), pY)
B <- fitter(X, B, Y, l1p = 0.01, l2p = 0.02, eps = 0.005, nits = 100, verb = TRUE)
B <- fitter(X, B, Y, l1p = 1e-3, l2p = 1e-3, eps = 0.001, nits = 100, verb = TRUE)
B <- fitter(X, B, Y, l1p = 1e-3, l2p = 1e-3, eps = 0.001, nits = 100, verb = TRUE)
predict_acc(X, B, Y, Yc)[1]
res_sp_gote <- predict_acc(Xte, B, Yte, Ycte)
res_sp_gote[1] #0.547

# saveRDS(B, "doubutsu1/prediction/spfit1_gote.rds")
