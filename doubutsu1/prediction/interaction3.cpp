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
  int ans = ncols3(p) - (j*(j-1)*(j-2))/6 + ((j-1)*(j-2))/6 - (k*(k-1))/2 + z-y;
  return (ans);
}





// [[Rcpp::export]]
NumericVector predict3(NumericMatrix x, NumericVector bt) {
  int nr = x.nrow();
  int nc = x.ncol();
  NumericVector y(nr);
  for (int i = 0; i < nr; i++) {
    double s = 0;
    NumericMatrix::Row rw = x(i, _);
    NumericVector v(rw);
    IntegerVector inds = nonzeroInds(v);
    int nv = inds.size();
    for (int jj = 0; jj < nv; jj++) {
      int j = inds[jj];
      int l = j;
      s = s + v[j] * bt[l];
    }
    for (int jj = 0; jj < (nv - 1); jj++) {
      for (int kk = (jj + 1); kk < nv; kk++) {
        int j = inds[jj];
        int k = inds[kk];
        int l = index2(nc, j, k);
        s = s + v[j] * v[k] * bt[l];
      }
    }
    for (int jj = 0; jj < (nv - 2); jj++) {
      for (int kk = (jj + 1); kk < (nv - 1); kk++) {
        for (int mm = (kk + 1); mm < nv; mm++) {
          int j = inds[jj];
          int k = inds[kk];
          int m = inds[mm];
          int l = index3(nc, j, k, m);
          s = s + v[j] * v[k] * v[m] * bt[l];
        }
      }
    }
    y(i) = s;
  }
  return (y);
}

// [[Rcpp::export]]
NumericVector gradient3(NumericVector mu, NumericMatrix x, NumericVector ws) {
  int nr = x.nrow();
  int nc = x.ncol();
  for (int i = 0; i < nr; i++) {
    NumericMatrix::Row rw = x(i, _);
    NumericVector v(rw);
    IntegerVector inds = nonzeroInds(v);
    int nv = inds.size();
    for (int jj = 0; jj < nv; jj++) {
      int j = inds[jj];
      int l = j;
      mu[l] = mu[l] + v[j] * ws[i];
    }
    for (int jj = 0; jj < (nv - 1); jj++) {
      for (int kk = (jj + 1); kk < nv; kk++) {
        int j = inds[jj];
        int k = inds[kk];
        int l = index2(nc, j, k);
        mu[l] = mu[l] + v[j] * v[k] * ws[i];
      }
    }
    for (int jj = 0; jj < (nv - 2); jj++) {
      for (int kk = (jj + 1); kk < (nv - 1); kk++) {
        for (int mm = (kk + 1); mm < nv; mm++) {
          int j = inds[jj];
          int k = inds[kk];
          int m = inds[mm];
          int l = index3(nc, j, k, m);
          mu[l] = mu[l] + v[j] * v[k] * v[m] * ws[i];
        }
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
# x <- pracma::randn(3)
# x[2, ] <- 0
# x[3, 2] <- 0
# bt <- rnorm(7)
# ws <- rep(1, 3)
# x2 <- cbind(x, x[, 1] * x[, 2], x[, 1] * x[, 3], x[, 2] * x[, 3], x[, 1] * x[, 2] * x[, 3])
# x2 %*% bt
# predict3(x, bt)
# bt0 <- 0 * bt
# bt0 + t(ws) %*% x2
# gradient3(bt0, x, ws)
# shrinker(-5:5, 0.1, 0.2)
# l0norm(c(0, 0.0, 2.0, -1))
# nonzeroInds(c(0, 0, 2, 1, 0, 0, 3, 0))

predictX <- predict3
gradientX <- gradient3
ncolsX <- ncols3
*/
