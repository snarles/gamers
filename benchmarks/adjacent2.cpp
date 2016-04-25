#include <Rcpp.h>
using namespace Rcpp;

//
// This version of the adjacent code uses fixed memory rather than copying the board.
//

// [[Rcpp::export]]
int makeMove(IntegerVector board, int n, int depth, int loc) {
  // check for loss
  if (board[loc] != 0) {
    return -1;
  }
  if (loc == 0 && board[1] != 0) {
    return -1;
  }
  if (loc == n && board[n] != 0) {
    return -1;
  }
  if (depth == 0) {
    return 0;
  }
  else {
    board[loc] = 1;
    int ans = -1;
    int i = 0;
    while (i < n && ans == 0) {
      int newans = -makeMove(board, n, depth - 1, i);
      if (newans > ans) {
        ans = newans;
      }
    }
    // undo move!
    board[loc] = 0;
    return(ans);
  }
}

// [[Rcpp::export]]
IntegerVector moveValues(IntegerVector board, int depth) {
  int n = board.size();
  int ans = 0;
  IntegerVector vals(n);
  for (int i = 0; i < n; i++) {
    vals[i] = makeMove(board, n, depth - 1, i);
  }
  return vals;
}



/*** R
mateCheck(c(0, 0, 0), 2, 2)
*/
