#include <Rcpp.h>
using namespace Rcpp;

//
// This version of the adjacent code uses fixed memory rather than copying the board.
//

// [[Rcpp::export]]
int makeMove(IntegerVector board, int n, int depth, int loc) {
  // check for loss
  if (board[loc] != 0) {
    Rprintf("|occ|");
    return -1;
  }
  if (loc == 0 && board[1] != 0) {
    Rprintf("|0a|");
    return -1;
  }
  if (loc == (n - 1) && board[(n - 2)] != 0) {
    Rprintf("|na|");
    return -1;
  }
  if (board[loc + 1] != 0 || board[loc - 1] != 0) {
    Rprintf("|adjc|");
    return -1;
  }
  if (depth == 0) {
    Rprintf("|ret0|");
    return 0;
  }
  Rprintf("DADA");
  board[loc] = 1;
  int ans = 1;
  int i = 0;
  while (i < n && ans == 0) {
    int newans = -makeMove(board, n, depth - 1, i);
    if (newans < ans) {
      ans = newans;
    }
  }
  // undo move!
  board[loc] = 0;
  return(ans);
}

// [[Rcpp::export]]
IntegerVector moveValues(IntegerVector board, int depth) {
  int n = board.size();
  IntegerVector vals(n);
  for (int i = 0; i < n; i++) {
    vals[i] = makeMove(board, n, depth - 1, i);
  }
  return vals;
}



/*** R
# R testing code
*/
