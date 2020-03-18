shuriken <- c(
                 1, 1, 2, 3, 5, 1, 6, 0, 
                 5, 5, 3, 1, 4, 5, 2, 0, 
                 4, 4, 1, 5, 6, 4, 3, 0, 
                 6, 6, 5, 4, 2, 6, 1, 0, 
                 2, 2, 4, 6, 3, 2, 5, 0, 
                 3, 3, 6, 2, 1, 3, 4, 0
)

next_pair <- function(v) {
  a_new <- (v[1] + shuriken[v[2]] - 1) %% 48 + 1
  b_new <- (v[2] + shuriken[a_new] - 1) %% 48 + 1
  return(c(a_new, b_new))
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

cycle1 <- compute_iterates(c(1,1), 2000)
cycle2 <- compute_iterates(c(1,2), 2000)
cycle3 <- compute_iterates(c(2,26), 2000)
cycle4 <- compute_iterates(c(3,27), 2000)

plot(cycle1, col = 'gray66')
points(cycle2, col='coral')
points(cycle3, col='blue')
points(cycle4, col='green')


t(cycle3)
#   2    8   14   15   17   20   23   24   25    27    27    31    36    41    44
#  26   26   31   33   37   42   45   45    3     8    13    14    20    23    25

t(cycle4)
#    3    8   13   14   20   23   25   26   26    31    33    37    42    45    45
#   27   27   31   36   41   44    2    8   14    15    17    20    23    24    25

