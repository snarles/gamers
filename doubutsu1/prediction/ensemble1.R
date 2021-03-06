## combine the policy and eval-based methods
source("doubutsu1/prediction/spolicy.R")
source("doubutsu1/lg_analysis_setup.R")
source("doubutsu1/prediction/spolicy.R")
source("doubutsu1/prediction/multinom2.R")
source("doubutsu1/prediction/multiclassreg.R")
Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")

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

####
##  Barplot
####

cts <- resTr$senteTurn
sort(unique(cts))
cp <- cts[res_sp_st$corrects==1]
ce <- cts[res_ev_st$corrects==1]
c2 <- cts[pred_sp==resTe$senteChosen]

ctab <- apply(t(sort(unique(cts))), 2, function(i) {
  c(sum(cts==i), sum(cp==i), sum(ce==i), sum(c2==i))
})

r1 <- ctab[1, ]/ctab[1, ]
r2 <- ctab[2, ]/ctab[1, ]
r3 <- ctab[3, ]/ctab[1, ]
r4 <- ctab[4, ]/ctab[1, ]
x <- 1:length(unique(cts)) * 5 - 4

colnames(ctab) <- paste("t", sort(unique(cts)), sep = "")
barplot(ctab, beside = TRUE, col = c("black", "red", "blue", "violet"))
lines(x, r1 * 500)
lines(x, r2 * 500, col = "red", lwd = 2)
lines(x, r3 * 500, col = "blue", lwd = 2)
lines(x, r4 * 500, col = "violet", lwd = 2)
