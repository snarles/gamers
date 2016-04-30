#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int ncols2(int sz) {
  return sz + ((sz - 1) * sz)/2;
}

// [[Rcpp::export]]
int index2(int p, int x, int y) {
  int j = p - x;
  int ans = (p - 1) + (p * (p - 1))/2 - (j * (j - 1))/2 + y - (p - j);
  return (ans);
}

// [[Rcpp::export]]
NumericVector predict2(NumericMatrix x, NumericVector bt) {
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
    for (int j = 0; j < (nc - 1); j++) {
      for (int k = (j + 1); k < nc; k++) {
        s = s + x(i, j) * x(i, k) * bt[l];
        l++;
      }
    }
    y(i) = s;
  }
  return (y);
}

// [[Rcpp::export]]
NumericVector gradient2(NumericVector mu, NumericMatrix x, NumericVector ws) {
  int nr = x.nrow();
  int nc = x.ncol();
  for (int i = 0; i < nr; i++) {
    int l = 0;
    for (int j = 0; j < nc; j++) {
      mu[l] = mu[l] + x(i, j) * ws[i];
      l++;
    }
    for (int j = 0; j < (nc - 1); j++) {
      for (int k = (j + 1); k < nc; k++) {
        mu[l] = mu[l] + x(i, j) * x(i, k) * ws[i];
        l++;
      }
    }
  }
  return (mu);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
// sourceCpp("doubutsu1/prediction/interaction.cpp")

/*** R
set.seed(0)
x <- pracma::randn(3)
bt <- rnorm(6)
ws <- rnorm(3)
x2 <- cbind(x, x[, 1] * x[, 2], x[, 1] * x[, 3], x[, 2] * x[, 3])
x2 %*% bt
predict2(x, bt)
bt + t(ws) %*% x2
gradient2(bt, x, ws)
*/
