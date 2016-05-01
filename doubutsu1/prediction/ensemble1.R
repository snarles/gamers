## combine the policy and eval-based methods

source("doubutsu1/prediction/spolicy.R")
source("doubutsu1/prediction/multinom2.R")
source("doubutsu1/prediction/multiclassreg.R")

## SENTE PREDICTION

bt1 <- readRDS("doubutsu1/prediction/multinom_fit2_sente.rds")
res_ev_st <- mcm_loss(resTe$senteAlts, resTe$senteChoice, resTe$senteMoves, bt1)
res_ev_st[1] # 0.566

B1 <- readRDS("doubutsu1/prediction/spfit1_sente.rds")
Xte <- resTe$senteX
Yte <- match(resTe$senteChosen, allmoves)
Ycte <- lapply(resTe$senteMoves, function(v) match(v, allmoves))
res_sp_st <- predict_acc(Xte, B1, Yte, Ycte)
res_sp_st[1] # 0.563

pmat_sp <- .5 * res_ev_st$pmat + .5 * res_sp_st$pmat

pred_sp <- legal_preds(pmat_sp, resTe$senteMoves)
sum(pred_sp==resTe$senteChosen)/length(resTe$senteChosen) # 0.611

## GOTE PREDICTION

bt2 <- readRDS("doubutsu1/prediction/multinom_fit2_gote.rds")
res_ev_gt <- mcm_loss(resTe$goteAlts, resTe$goteChoice, resTe$goteMoves, bt2)
res_ev_gt[1] # 0.579

B2 <- readRDS("doubutsu1/prediction/spfit1_gote.rds")
Xte <- resTe$goteX
Yte <- match(resTe$goteChosen, allmoves)
Ycte <- lapply(resTe$goteMoves, function(v) match(v, allmoves))
res_sp_gt <- predict_acc(Xte, B2, Yte, Ycte)
res_sp_gt[1] # 0.547

pmat_gt <- .5 * res_ev_gt$pmat + .5 * res_sp_gt$pmat

pred_gt <- legal_preds(pmat_gt, resTe$goteMoves)
sum(pred_gt==resTe$goteChosen)/length(resTe$goteChosen) # 0.585
