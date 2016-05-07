#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int ncols1(int sz) {
  return sz;
}


// [[Rcpp::export]]
NumericVector shrinker(NumericVector bt, double l1p, double l2p) {
  int n = bt.size();
  for (int i = 0; i < n; i++) {
    if (bt[i] >= -l1p && bt[i] <= l1p) {
      bt[i] = 0;
    }
    if (bt[i] > l1p) {
      bt[i] = bt[i] - l1p;
    }
    if (bt[i] < - l1p) {
      bt[i] = bt[i] + l1p;
    }
    bt[i] = bt[i] * (1 - l2p);
  }
  return (bt);
}

// [[Rcpp::export]]
NumericVector predict1(NumericMatrix x, NumericVector bt) {
  int nr = x.nrow();
  int nc = x.ncol();
  NumericVector y(nr);
  for (int i = 0; i < nr; i++) {
    double s = 0;
    int l = 0;
    for (int j = 0; j < nc; j++) {
      s = s + x(i, j) * bt[l];
      l++;
    }
    y(i) = s;
  }
  return (y);
}

// [[Rcpp::export]]
NumericVector gradient1(NumericVector mu, NumericMatrix x, NumericVector ws) {
  int nr = x.nrow();
  int nc = x.ncol();
  for (int i = 0; i < nr; i++) {
    int l = 0;
    for (int j = 0; j < nc; j++) {
      mu[l] = mu[l] + x(i, j) * ws[i];
      l++;
    }
  }
  return (mu);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
// sourceCpp("doubutsu1/prediction/interaction.cpp")

/*** R
# x <- pracma::randn(3)
# bt <- rnorm(3)
# ws <- rnorm(3)
# x2 <- x
# x2 %*% bt
# predict1(x, bt)
# bt + t(ws) %*% x2
# gradient1(bt, x, ws)
# c(-(5:1) + 0.1, 0, (1:5) - 0.1) * (1 - 0.2)
# shrinker(-5:5, 0.1, 0.2)
*/
