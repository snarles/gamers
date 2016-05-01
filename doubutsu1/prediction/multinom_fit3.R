
source("doubutsu1/prediction/multinom2.R")

Rcpp::sourceCpp("doubutsu1/prediction/interaction3.cpp")


mats <- resTr$senteAlts
choice <- resTr$senteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 3e-4, l2p = 1e-4, eps = 0.1)
mcm_loss(mats, choice, bt)[1]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1]  # 0.439


plot(bt)

mats <- resTr$goteAlts
choice <- resTr$goteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 3e-4, l2p = 1e-4, eps = 0.1)
mcm_loss(mats, choice, bt)[1]
mcm_loss(resTe$goteAlts, resTe$goteChoice, bt)[1] # 0.465
