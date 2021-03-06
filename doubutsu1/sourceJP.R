## #SET THIS AS DIRECTORY TO DOBUTSU PROGRAM
## https://dell.tanaka.ecc.u-tokyo.ac.jp/~ktanaka/dobutsushogi/

SOLVER_DIR <- "~/Downloads/dobutsu"

catn <- function(x) cat(paste0(x, "\n"))

PIECESTRSjp <- cbind(c(" . ", "+LI", "+KI", "+ZO", "+HI", "+NI"), 
                     c(" . ", "-LI", "-KI", "-ZO", "-HI", "-NI"))

LOXjp <- c("A1", "B1", "C1", 
           "A2", "B2", "C2",
           "A3", "B3", "C3",
           "A4", "B4", "C4"
)

print_state_JP <- function(state, blind = FALSE) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  movev <- state[49:51]
  strs <- rep("", 12)
  pl <- ((state[4] - 1) %% 2) + 1
  for (i in 1:12) {
    str <- PIECESTRSjp[board[1, i] + 1 + board[3, i], board[2, i] + 1]
    strs[i] <- str
  }
  mat <- t(matrix(strs, nrow = 3))
  handst <- paste(c(hand1[c(4,3,2)], hand2[c(4,3,2)]), collapse = "")
  for (i in 1:4) {
    cat(paste(mat[i, ], collapse = ""))
    catn("")
  }
  catn(handst)
  catn(c("-", "+")[pl])
}

# state <- states[300, ]
# print_state(state)
# print_state_JP(state)

solve_state_raw <- function(state, print = FALSE) {
  odir <- getwd()
  setwd(SOLVER_DIR)
  sink("temp.txt")
  print_state_JP(state)
  sink()
  system("./checkState temp.txt 2> out.txt")
  ff <- readLines("out.txt")
  setwd(odir)
  if (print) {
    for (ll in ff) {
      catn(ll)
    }
  }
  ff  
}

get_annotation_raw <- function(jgame, print = FALSE) {
  odir <- getwd()
  setwd(SOLVER_DIR)
  sink("temp.txt")
  for (m in jgame) catn(m)
  sink()
  system("./checkcsa temp.txt 2> out.txt")
  ff <- readLines("out.txt")
  setwd(odir)
  if (print) {
    for (ll in ff) {
      catn(ll)
    }
  }
  ff
}

cats <- function(v) {
  for (w in v) catn(w)
}

typesJP <- c("LI"=1, "KI"=2, "ZO"=3, "HI"=4, "NI"=4)
promJP <- c("LI"=0, "KI"=0, "ZO"=0, "HI"=0, "NI"=1)
transJP <- c("LI"="K", "KI"="R", "ZO"="B",  "HI"="P", "NI"="P")
ltransJP <- c("A1"="3a", "B1"="2a", "C1"="1a",
              "A2"="3b", "B2"="2b", "C2"="1b",
              "A3"="3c", "B3"="2c", "C3"="1c",
              "A4"="3d", "B4"="2d", "C4"="1d", "00"="**")

## get probabilities
analysis_to_values <- function(v) {
  mstarts <- which(v == "------------------")
  nsections <- length(mstarts)
  mstarts <- c(mstarts, length(v) + 1)
  i <- 1
  res <- list()
  for (i in 1:nsections) {
    section <- v[mstarts[i]:(mstarts[i+1] - 1)]  
    board <- section[2:5]
    boardv <- parse_board(board)
    hand <- section[6]
    handv <- parse_hand(hand)
    turn <- section[7]
    turnv <- c("+"=0, "-"=1)[turn]
    valu <- section[9]
    valv <- as.numeric(strsplit(valu, "\\(")[[1]][1])
    state0 <- c(valv, 0, 0, turnv, boardv, handv, 0, 0, 0)
    names(state0) <- NULL
    tree <- t(state0)
    ## cats(section[2:9]); draw_state(state0)
    if (length(section) > 8) {
      moves0 <- section[10:(length(section))]
      if(length(grep("Move", moves0)) > 0) moves0 <- moves0[-length(moves0)]
      tree <- build_tree(state0, 1, length(moves0)+10)
      tree[-1, 1] <- 2 * turnv - 1
      tree[1, 1] <- valv
      moves_in_tree <- apply(tree[-1, 49:51, drop = FALSE], 1, movestr)
      rownames(tree) <- c("?", moves_in_tree)
      moves <- sapply(moves0, function(v) {
        strsplit(v, " ")[[1]][3]
      }, USE.NAMES = FALSE)
      moves <- sapply(moves, parse_moveJP, USE.NAMES = FALSE)
      setdiff(moves, moves_in_tree)
      matchinds <- match(moves, moves_in_tree) + 1
      vals <- as.numeric(sapply(moves0, function(v) {
        strsplit(strsplit(v, " ")[[1]][4], "\\(")[[1]][1]
      }, USE.NAMES = FALSE))
      parens <- as.numeric(sapply(moves0, function(v) {
        w <- strsplit(strsplit(v, " ")[[1]][4], "\\(")[[1]][2]
        substr(w, 1, nchar(w)-1)
      }))
      if ((turnv==0 && valv==-1) || (turnv==1 && valv==1))
      {
        valvs <- rep(valv, length(vals))
      } else {
        valvs <- -vals * valv  ## don't know why they encoded this way..
      }
      if (valv==0) {
        loseval <- (2 * turnv) - 1
        v2 <- apply(tree, 1, function(v) maxVal(v, 3))
        tree[, 1] <- 0
        tree[v2!=0, 1] <- loseval
        filt <- !is.na(matchinds)
        tree[matchinds[filt], 2] <- parens[filt]
        tree[matchinds[filt & parens != 0], 1] <- loseval
        res[[i]] <- tree
        return(res)
      }
      filt <- !is.na(matchinds)
      tree[matchinds[filt], 1] <- valvs[filt]
      tree[matchinds[filt], 2] <- parens[filt]
      res[[i]] <- tree
    }
  }
  res
}

parse_board <- function(board) {
  pieces <- 
    sapply(board, function(s) c(substr(s, 1, 3), substr(s, 4, 6), substr(s, 7, 9)),
           USE.NAMES = FALSE)
  boardv <- numeric(36)
  for (stt in 1:12) {
    pz <- pieces[stt]
    if (pz==" . ") {
      # do nothing
    } else {
      boardv[stt * 3 - 1] <- c("+"=0, "-"=1)[substr(pz, 1, 1)]
      boardv[stt * 3 - 2] <- c("LI"=1,"KI"=2,"ZO"=3,"HI"=4,"NI"=4)[substr(pz, 2, 3)]
      if (substr(pz, 2, 3)=="NI") boardv[stt*3] <- 1
    }
  }
  boardv
}

parse_hand <- function(hand) {
  cs <- as.numeric(strsplit(hand, NULL)[[1]])
  handv <- c(0, cs[3], cs[2], cs[1], 0, cs[6], cs[5], cs[4])
  handv
}

parse_moveJP <- function(chosen) {
  l1 <- substr(chosen , 2, 3)
  l2 <- substr(chosen , 4, 5)
  l3 <- substr(chosen, 6, 7)
  (mv <- paste0(transJP[l3], "-", ltransJP[l1], "-", ltransJP[l2]))
  mv
}

move_to_JP <- function(mv, pl, prom = 0) {
  pcs <- strsplit(mv, "-")[[1]]
  spl <- c("+", "-")[pl + 1]
  if (prom == 0) {
    pc <- names(transJP)[transJP == pcs[1]][1]
  } else {
    pc <- names(transJP)[transJP == pcs[1]][2]
  }
  paste0(spl, 
         names(ltransJP)[ltransJP == pcs[2]], 
         names(ltransJP)[ltransJP == pcs[3]], pc)
}

alt_df_2s <- function(df, ind) {
  paste0(rownames(df)[ind], " (", df[ind, 1], ":", df[ind, 2], ")")
}

analyze_game <- function(game, MODE = c("print", "draw", "return"),
                         extra_pause = FALSE) {
  MODE <- MODE[1]
  states <- statesFromGame(game)
  jgame <- c()
  for (i in 1:length(states)) {
    state <- states[[i]]
    board <- matrix(state[5:40], nrow = 3)
    hand1 <- state[41:44]
    hand2 <- state[45:48]
    movev <- state[49:51]
    prom <- 0
    if (movev[1] > 0 && board[3, movev[3]]==1) prom <- 1
    jgame[i] <- move_to_JP(game[i], (i + 1) %% 2, prom)
  }
  ff <- get_annotation_raw(jgame)
  divs <- which(sapply(ff, function(v) substr(v, 1, 3)=="0 :"))
  divs <- c(divs, length(ff) + 1)
  dlist <- list()
  for (i in 1:length(states)) {
    mstr <- paste(i, game[i], sep = ". ")
    br <- ff[divs[i]:(divs[i + 1] - 1)]
    ind <- which(sapply(br, function(v) substr(v, 1, 4) == "Move"))
    br <- br[1:ind]
    br <- sapply(br, function(v) strsplit(v, " ")[[1]][3])
    jpmv <- br[ind]
    br <- br[1:(ind - 1)]
    altmvs <- sapply(br, substr, 1, 7)
    wl <- sapply(br, substr, 12, 18)
    wl1 <- as.numeric(sapply(wl, function(v) strsplit(v, "\\(")[[1]][1]))
    wl2 <- sapply(wl, function(v) as.numeric(sub(")", "", strsplit(v, "\\(")[[1]][2])))
    wstr <- c("W", "D", "L")[wl1 + 2]
    vals <- wl1/(1 + wl2)
    o <- order(vals)
    altmvs <- sapply(altmvs, parse_moveJP)
    df <- data.frame(value = wstr, time = wl2)
    rownames(df) <- altmvs
    df <- df[o, ]
    dlist[[mstr]] <- df
    chose_ind <- which(rownames(df)==game[i])
    if (chose_ind == 1) {
      printst <- paste0(i, ". ", alt_df_2s(df, chose_ind))
    } else {
      printst <- paste0(i, ". ", alt_df_2s(df, chose_ind), " [", alt_df_2s(df, 1), "]")
    }
    if (MODE == "draw") {
      if (i == 1) {
        draw_state(init_state, title = TRUE)
      } else {
        draw_state(states[[i - 1]], title = TRUE)
      }
      if (extra_pause) locator(1)
      print(dlist[i])
      title(sub = printst)
      locator(1)
    }
    if (MODE == "print") {
      if (i == 1) {
        print_state(init_state)
      } else {
        print_state(states[[i - 1]])
      }
      if (extra_pause) readline("             (Continue):")
      catn(printst)
      readline("             (Continue):")
    }
  }
  if (MODE == "return") return(dlist)
}

# analyze_game(glist[[2]], MODE = "draw", TRUE)


solve_game_disp <- function(problem, extra_pause = FALSE) {
  #draw_state(problem, title = TRUE)
  res <- solve_state_raw(problem)
  res2 <- analysis_to_values(res)
  for (i in 1:length(res2)) {
    v <- res2[[i]]
    draw_state(v[1, ], title = TRUE)
    if (extra_pause) locator(1)
    catn(" ")
    mn <- v[-1, 1:2, drop = FALSE]
    vals <- mn[, 1]/(1 + mn[, 2])
    if (v[1, 4] %% 2 == 0) vals <- -vals
    mn <- mn[order(vals), , drop = FALSE]
    print(mn)
    title(sub = paste0(
      paste(rownames(mn)[sort(vals) == min(vals)], collapse = ", "),
      " (", mn[1, 2], ")"))
    locator(1)
  }
  invisible(res2)
}