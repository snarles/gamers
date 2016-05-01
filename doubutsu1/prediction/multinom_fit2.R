
source("doubutsu1/prediction/multinom2.R")

Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")


mats <- resTr$senteAlts
choice <- resTr$senteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 1e-5, l2p = 1e-5, eps = 0.1)
mcm_loss(mats, choice, bt)[1]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1]  # 0.532


plot(bt)

mats <- resTr$goteAlts
choice <- resTr$goteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 1e-5, l2p = 1e-5, eps = 0.1)
mcm_loss(mats, choice, bt)[1]
mcm_loss(resTe$goteAlts, resTe$goteChoice, bt)[1] # 0.524
