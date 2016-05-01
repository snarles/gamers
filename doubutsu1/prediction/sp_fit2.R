source("doubutsu1/prediction/spolicy.R")
source("doubutsu1/prediction/multiclassreg2.R")

# library(parallel)

pY <- length(allmoves)
Y <- match(resTr$senteChosen, allmoves)
Yc <- lapply(resTr$senteMoves, function(v) match(v, allmoves))
X <- resTr$senteX
Xte <- resTe$senteX
Yte <- match(resTe$senteChosen, allmoves)
Ycte <- lapply(resTe$senteMoves, function(v) match(v, allmoves))
B <- matrix(0, ncolsX(ncol(X)), pY)
B <- gdes(X, B, Y, l1p = 0, l2p = 0, eps = 0.001)
loglik(X, B, Y)
B <- fitter(X, B, Y, l1p = 0.01, l2p = 0.02, eps = 0.005, nits = 100, verb = TRUE)
B <- fitter(X, B, Y, l1p = 1e-3, l2p = 1e-3, eps = 0.001, nits = 100, verb = TRUE)
B <- fitter(X, B, Y, l1p = 1e-3, l2p = 1e-3, eps = 0.001, nits = 100, verb = TRUE)
predict_acc(X, B, Y, Yc)
predict_acc(Xte, B, Yte, Ycte) # 0.559

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
predict_acc(X, B, Y, Yc)
predict_acc(Xte, B, Yte, Ycte) #0.547

