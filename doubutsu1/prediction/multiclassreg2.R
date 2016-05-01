Rcpp::sourceCpp("doubutsu1/prediction/interaction2a.cpp")

shrinkerP <- function(bt, l1p, l2p) {
  s <- sign(bt)
  filt <- abs(bt) > l1p
  bt[!filt] <- 0
  bt[filt] <- bt[filt] - s[filt] * l1p
  bt <- bt - l2p * bt
  bt
}

mpreds <- function(X, B) {
  ps <- matrix(0, nrow(X), ncol(B))
  for (i in 1:ncol(B)) {
    ps[, i] <- predictX(X, B[, i])
  }
  ps <- t(apply(ps, 1, function(v) exp(v)/sum(exp(v))))
  ps
}

loglik <- function(X, B, Y) {
  ps <- mpreds(X, B)
  sum(log(ps[cbind(1:length(Y), Y)]))
}

gdes <- function(X, B, Y, l1p=0, l2p=0, eps=0.1) {
  ps <- mpreds(X, B)
  ps[cbind(1:length(Y), Y)] <- ps[cbind(1:length(Y), Y)] - 1
  for (i in 1:ncol(B)) {
    B[, i] <- gradientX(B[, i], X, -eps * ps[, i])
  }
  B <- shrinkerP(B, eps*l1p, eps*l2p)
  B
}

predict_acc <- function(X, B, Y, Yc) {
  ps <- mpreds(X, B)
  Yh <- 0 * Y
  probs <- 0 * Y
  for (i in 1:length(Y)) {
    ps[i, -Yc[[i]]] <- -Inf
    Yh[i] <- which(ps[i, ]==max(ps[i, ]))[1]
    probs[i] <- ps[i, Yh[i]]
  }
  list(acc = sum(Yh==Y)/length(Y), logloss = sum(log(probs)))
}

