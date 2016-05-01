#include <Rcpp.h>
using namespace Rcpp;


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
int l0norm(NumericVector x) {
  int n = x.size();
  int s = 0;
  for (int i = 0; i < n; i++) {
    if (x[i] != 0) {
      s++;
    }
  }
  return (s);
}

// [[Rcpp::export]]
IntegerVector nonzeroInds(NumericVector x) {
  IntegerVector ans(l0norm(x));
  int n = x.size();
  int s = 0;
  for (int i = 0; i < n; i++) {
    if (x[i] != 0) {
      ans[s] = i;
      s++;
    }
  }
  return (ans);
}



// [[Rcpp::export]]
int ncols3(int sz) {
  return sz + ((sz - 1) * sz)/2 + (sz * (sz - 1) * (sz - 2))/6;
}

// [[Rcpp::export]]
int index2(int p, int x, int y) {
  int j = p - x;
  int ans = (p - 1) + (p * (p - 1))/2 - (j * (j - 1))/2 + y - (p - j);
  return (ans);
}

// [[Rcpp::export]]
int index3(int p, int x, int y, int z) {
  int j = p - x;
  int k = p - y;
  int ans = ncols3(p) - (j*(j-1)*(j-2))/6 + ((j-1)*(j-2))/6 - (k*(k-1))/2 + z-y + 1;
  return (ans);
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
// sourceCpp("doubutsu1/prediction/interaction.cpp")

/*** R
x <- pracma::randn(3)
bt <- rnorm(7)
ws <- rnorm(3)
x2 <- cbind(x, x[, 1] * x[, 2], x[, 1] * x[, 3], x[, 2] * x[, 3], x[, 1] * x[, 2] * x[, 3])
x2 %*% bt
predict3(x, bt)
bt + t(ws) %*% x2
gradient3(bt, x, ws)
shrinker(-5:5, 0.1, 0.2)
l0norm(c(0, 0.0, 2.0, -1))
nonzeroInds(c(0, 0, 2, 1, 0, 0, 3, 0))

predictX <- predict3
gradientX <- gradient3
ncolsX <- ncols3
*/
