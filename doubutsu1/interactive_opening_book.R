####
##  CPU vs itself
####
# alt_summary <- function(alt) {
#   v <- alt[1, ]
#   mn <- alt[-1, 1:2, drop = FALSE]
#   vals <- mn[, 1]/(1 + mn[, 2])
#   if (v[4] %% 2 == 0) vals <- -vals
#   mn <- mn[order(vals), , drop = FALSE]
#   list(state = v, mn = mn)
# }
# alts <- readRDS("doubutsu1/solved_alts2.rds")
# altu <- sapply(alts, function(v) uhash2(v[1, ]))
# salts <- lapply(alts, alt_summary)
# names(salts) <- altu
# saveRDS(salts, "doubutsu1/salts.rds")

# salts <- lapply(alts, alt_summary)
# > length(altu)
# [1] 49225
# > length(unique(altu))
# [1] 49225
# > saveRDS(altu, "doubutsu1/old_altu.rds")

cat("Loading...")
cat("\n")

draw_salt <- function(salt) {
  draw_state(salt$state, title = TRUE)
  print(salt$mn)
}

library(Rcpp)
# load("doubutsu1/vs_cpu.rda")
source("doubutsu1/set_up_vs_cpu.R")
sourceCpp("doubutsu1/Rsource.cpp")
sourceCpp("doubutsu1/Rsource2.cpp")
source("doubutsu1/viz.R")
source("doubutsu1/gui.R")
salts <- readRDS("doubutsu1/salts.rds")

opening_moves_from_book <- function() {
  filt <- database[, 4] == 1
  inds <- which(filt)
  st <- database[inds, , drop = FALSE]
  mvs <- apply(st[, 49:51, drop = FALSE], 1, movestr)
  #print(sort(table(mvs), TRUE))
  sort(table(mvs), TRUE)
}

database <- rbind(database, init_state)

next_moves_from_book <- function(state) {
  pl <- state[4] %% 2
  h <- hash_state(state)
  filt <- (hashes == h) & (statepl == pl)
  inds <- which(filt)
  if (length(inds) == 0) return("unknown")
  if (length(inds) > 1) {
    inds <- inds[database[inds, 4] == (database[inds + 1, 4] - 1)]
    if (length(inds) > 1) {
      mvs <- apply(database[inds + 1, 49:51, drop = FALSE], 1, movestr)
      #print(sort(table(mvs), TRUE))
      return(sort(table(mvs), TRUE))
    }
  }
  return ("resign")
}

flag <- TRUE
state <- init_state
prev_states <- list()
redraw <- TRUE
while(flag) {
  if (redraw) {
    draw_state(state, title = TRUE)
    redraw <- FALSE
    if (state[4] == 0) {
      mvs <- opening_moves_from_book()
    } else {
      mvs <- next_moves_from_book(state)
    }
  }
  catn("")
  for (i in 1:length(mvs)) {
    catn(paste0(i, " : ", names(mvs)[i], " (", mvs[i], ")"))
  }
  catn("B : Back")
  catn("A : Analyze")
  catn("X : Exit")
  ln <- readline("Choose: ")
  if (ln == "X") flag <- FALSE
  if (ln == "B") {
    if (length(prev_states) > 0) {
      state <- prev_states[[1]]
      prev_states <- prev_states[-1]
    } else {
      state <- init_state
    }
    redraw <- TRUE
  }
  if (ln == "A") {
    uh <- uhash2(state)
    print(salts[[uh]]$mn)
    readline("   (OK) : ")
  }
  if (ln %in% paste(1:length(mvs))) {
    prev_states <- c(list(state), prev_states)
    mv <- names(mvs)[as.numeric(ln)]
    state <- move_parser(state, mv)
    redraw <- TRUE
  }
}


