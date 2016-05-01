
source("doubutsu1/prediction/multinom2.R")

Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")


mats <- resTr$senteAlts
choice <- resTr$senteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 2e-5, l2p = 1e-5, eps = 0.01)
mcm_loss(mats, choice, bt)[1]
mcm_loss(resTe$senteAlts, resTe$senteChoice, bt)[1]  # 0.566

saveRDS(bt, "doubutsu1/prediction/multinom_fit2_sente.rds")

mats <- resTr$goteAlts
choice <- resTr$goteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 2e-5, l2p = 1e-5, eps = 0.01)
mcm_loss(mats, choice, bt)[1]
mcm_loss(resTe$goteAlts, resTe$goteChoice, bt)[1] # 0.579

saveRDS(bt, "doubutsu1/prediction/multinom_fit2_gote.rds")
