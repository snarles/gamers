#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector move(IntegerVector state, int start, int end, int prom) {
  IntegerVector state2(clone(state));
  int startind = 1 + start * 3;
  int endind = 1 + end * 3;
  state2[3] = state[3] + 1;
  state2[48] = state[startind];
  state2[49] = start;
  state2[50] = end;
  if (state[endind] > 0) {
    int ptype = state[endind];
    int pl = state[endind + 1];
    int handind = ptype + (1 - pl) * 4 + 39;
    state2[handind] += 1;
  }
  for (int k = 0; k < 3; k++) {
    state2[endind + k] = state[startind + k];
    state2[startind + k] = 0;
  }
  if (prom == 1) {
    state2[endind + 2] = 1;
  }
  return state2;
}

// [[Rcpp::export]]
IntegerVector dropp(IntegerVector state, int pl, int ptype, int end) {
  IntegerVector state2(clone(state));
  state2[48] = ptype;
  state2[49] = 0;
  state2[50] = end;
  int endind = 1 + end * 3;
  state2[3] = state[3] + 1;
  int handind = ptype + pl * 4 + 39;
  state2[handind] += -1;
  state2[endind] = ptype;
  state2[endind + 1] = pl;
  state2[endind + 2] = 0;
  return state2;
}

IntegerVector addDrop(IntegerVector cstate, int pl, int ptype, int end) {
  int endind = 1 + end * 3;
  int occ = cstate[endind];
  if (occ == 0) {
    cstate[1] = 0;
    IntegerVector state2 = dropp(cstate, pl, ptype, end);
    return state2;
  }
  cstate[1] = -1;
  return cstate;
}

IntegerVector stepMove(IntegerVector cstate, int start, int vert, int horz) {
  int startind = 1 + start * 3;
  int x = (start - 1) % 3 + 1;
  int y = (start - x)/3 + 1;
  cstate[1] = -1;
  int pl = cstate[startind + 1];
  if (pl == 0) {
    vert = -vert;
    horz = -horz;
  }
  int x2 = x + horz;
  int y2 = y + vert;
  if ((x2 < 1) || (x2 > 3) || (y2 < 1) || (y2 > 4)) {
    return cstate;
  }
  int end = (y2 - 1)*3 + x2;
  int endind = 1 + end * 3;
  int endtype = cstate[endind];
  int endpl = cstate[endind + 1];
  if((endtype != 0) && (endpl == pl)) {
    return cstate;
  }
  int prom = 0;
  // handle pawn promotion
  if((cstate[startind] == 4) && (y2 == 1 + 3 * pl)) {
    prom = 1;
  }
  cstate[1] = 0;
  return move(cstate, start, end, prom);
}

// [[Rcpp::export]]
int maxVal(IntegerVector cstate, int maxturn) {
  int turn = cstate[3];
  int pl = turn % 2;
  if (cstate[40] > 0) return -1;
  if (cstate[44] > 0) return -1;
  if (pl == 0) {
    if (cstate[4] == 1 && cstate[5] == 0) return 1;
    if (cstate[7] == 1 && cstate[8] == 0) return 1;
    if (cstate[10] == 1 && cstate[11] == 0) return 1;
  }
  else {
    if (cstate[31] == 1 && cstate[32] == 1) return 1;
    if (cstate[34] == 1 && cstate[35] == 1) return 1;
    if (cstate[37] == 1 && cstate[38] == 1) return 1;
  }
  if (turn == maxturn) {
    return 0;
  }
  int ans = -1;
  for (int ptype = 1; ptype < 5; ptype++) {
    int handind = ptype + pl * 4 + 39;
    if (cstate[handind] > 0) {
      for (int end = 1; end < 13; end++) {
        IntegerVector newstate = addDrop(cstate, pl, ptype, end);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
      }
    }
  }
  // look for moves
  for (int start = 1; start < 13; start++) {
    int startind = 1 + start * 3;
    int ptype = cstate[startind];
    int prom = cstate[startind + 2];
    if ((ptype != 0) && (cstate[startind + 1]==pl)) {
      if (ptype==1) {
        IntegerVector newstate = stepMove(cstate, start, 1, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 1, 0);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 1, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 0, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 0, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, -1, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, -1, 0);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, -1, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
      }
      if (ptype==2) {
        IntegerVector newstate = stepMove(cstate, start, 1, 0);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 0, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 0, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, -1, 0);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
      }
      if (ptype==3) {
        IntegerVector newstate = stepMove(cstate, start, 1, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 1, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, -1, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, -1, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
      }
      if (ptype==4 && prom == 0) {
        IntegerVector newstate = stepMove(cstate, start, 1, 0);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
      }
      if (ptype==4 && prom == 1) {
        IntegerVector newstate = stepMove(cstate, start, 1, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 1, 0);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 1, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 0, 1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, 0, -1);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
        newstate = stepMove(cstate, start, -1, 0);
        if (newstate[1] != -1) {
          ans = std::max(ans, -maxVal(newstate, maxturn));
        }
        if (ans == 1) return 1;
      }
    }
  }
  return ans;
}

IntegerVector hashState0(IntegerVector state) {
  IntegerVector hh(3);
  for (int lp = 0; lp < 2; lp ++) {
    int s = 0;
    int p = 1;
    for (int start = 1 + lp * 6; start < 7 + lp * 6; start++) {
      int startind = 1 + start * 3;
      int pcode = state[startind] + state[startind + 2] + 5 * state[startind + 1];
      s = s + p * pcode;
      p = p * 11;
    }
    hh[lp] = s;
  }
  int s2 = 0;
  int p = 1;
  for (int k = 40; k < 48; k++) {
    s2 = s2 + p * state[k];
    p = p * 3;
  }
  hh[2] = s2;
  return hh;
}

IntegerVector swapPos(IntegerVector state, int start, int end) {
  IntegerVector placeholder(3);
  int startind = 1 + start * 3;
  int endind = 1 + end * 3;
  for (int k = 0; k < 3; k++) {
    placeholder[k] = state[startind + k];
    state[startind + k] = state[endind + k];
    state[endind + k] = placeholder[k];
  }
  return state;
}

IntegerVector flipState(IntegerVector state) {
  //flip the board
  IntegerVector placeholder(4);
  for (int k = 0; k < 4; k++) {
    placeholder[k] = state[40 + k];
    state[40 + k] = state[44 + k];
    state[44 + k] = placeholder[k];
  }
  for (int k = 1; k < 7; k++) {
    state = swapPos(state, k, 13 - k);
  }
  for (int start = 1; start < 13; start++) {
    int startind = 1 + start * 3;
    if (state[startind] != 0) {
      state[startind + 1] = 1 - state[startind + 1];
    }
  }
  return state;
}

IntegerVector mirrorState(IntegerVector state) {
  //LR flip the board
  state = swapPos(state, 1, 3);
  state = swapPos(state, 4, 6);
  state = swapPos(state, 7, 9);
  state = swapPos(state, 10, 12);
  return(state);
}

//[[Rcpp::export]]
IntegerVector hashState(IntegerVector state0) {
  IntegerVector state(clone(state0));
  //IntegerVector hh2(3);
  int pl = state[3] % 2;
  if (pl == 1) {
    state = flipState(state);
  }
  IntegerVector hh1 = hashState0(state);
  IntegerVector hh2 = hashState0(mirrorState(state));
  // compare hh1 and hh2, take the lesser
  int diff = 0;
  int p = 1;
  for (int k = 2; k > -1; k--) {
    if (hh1[k] < hh2[k]) {
      diff -= p;
    }
    else {
      diff += p;
    }
    p = 10 * p;
  }
  if (diff < 0) {
    return hh1;
  }
  else {
    return hh2;
  }
}
