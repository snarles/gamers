
for (nbase in 2:20) {
  #print(list(nbase=nbase))
  next_pair <- function(v) {
    a_new <- (v[1] + v[2]) %% nbase
    return(c(v[2], a_new))
  }
  
  compute_iterates <- function(v, n_its) {
    iterates <- matrix(0, n_its, 2)
    for (i in 1:n_its) {
      iterates[i, ] <- v
      v <- next_pair(v)
    }
    print(dim(unique(iterates))[1])
    iterates[1:dim(unique(iterates))[1],]
  }
  
  cycle1 <- compute_iterates(c(1,1), 3000)
  
}
