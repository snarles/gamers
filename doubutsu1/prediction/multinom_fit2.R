
source("doubutsu1/prediction/multinom2.R")

Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")


mats <- resTr$senteAlts
choice <- resTr$senteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 2e-5, l2p = 1e-5, eps = 0.01)
mcm_loss(mats, choice, resTr$senteMoves, bt)[1]
res_st <- mcm_loss(resTe$senteAlts, resTe$senteChoice, resTe$senteMoves, bt)
res_st[1]# 0.566

# saveRDS(bt, "doubutsu1/prediction/multinom_fit2_sente.rds")

mats <- resTr$goteAlts
choice <- resTr$goteChoice
bt <- numeric(ncolsX(ncol(mats[[1]])))
bt <- mcm_sgd(mats, choice, bt, l1p = 2e-5, l2p = 1e-5, eps = 0.01)
mcm_loss(mats, choice, resTr$goteChoice, bt)[1]
res_gt <- mcm_loss(resTe$goteAlts, resTe$goteChoice, resTe$goteMoves, bt)
res_gt[1] # 0.579

# saveRDS(bt, "doubutsu1/prediction/multinom_fit2_gote.rds")
