source("doubutsu1/prediction/spolicy.R")
source("doubutsu1/prediction/multiclassreg.R")

# library(parallel)

pY <- length(allmoves)
Y <- match(resTr$senteChosen, allmoves)
Yc <- lapply(resTr$senteMoves, function(v) match(v, allmoves))
X <- resTr$senteX
B <- matrix(0, ncol(X), pY)
loglik(X, B, Y)
B <- gdes(X, B, Y, l1p = 0.01, l2p = 0.02, eps = 0.001)
loglik(X, B, Y)
predict_acc(X, B, Y, Yc)
