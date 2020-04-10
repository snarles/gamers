#mult 4 version
shuriken <- c(
  1, 1, 2, 3, 5, 1, 6, 0, 
  3, 3, 6, 2, 1, 3, 4, 0,
  2, 2, 4, 6, 3, 2, 5, 0, 
  6, 6, 5, 4, 2, 6, 1, 0, 
  4, 4, 1, 5, 6, 4, 3, 0, 
  5, 5, 3, 1, 4, 5, 2, 0
)

shuriken <- floor(runif(24) * 6)
shuriken <- c(1,1,2,3,0,
              3,3,1,4,0,
              4,4,3,2,0,
              2,2,4,1,0) # base 5
shuriken <- c(1,1,2,3,5,2,1,3,4,1,5,0,5,5,4,3,1,4,5,3,2,5,1,0) # base 6
# base 10
shuriken <- c(1,1,2,3,5,8,3,1,4,5,9,4,3,7,0,7,7,4,1,5,6,1,7,8,5,3,8,1,9,0,9,9,8,7,5,2,7,9,6,5,1,6,7,3,0,3,3,6,9,5,4,9,3,2,5,7,2,9,1,0,1,2,3,5,8,3,1,4,5,9,4,3,7,0,7,7,4,1,5,6,1,7,8,5,3,8,1,9,0,9,9,8,7,5,2,7,9,6,5,1,6,7,3,0,3,3,6,9,5,4,9,3,2,5,7,2,9,1,0,1)

next_pair <- function(v) {
  a_new <- (v[1] + shuriken[v[2]] - 1) %% length(shuriken) + 1
  #b_new <- (v[2] + shuriken[a_new] - 1) %% length(shuriken) + 1
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

cycle1 <- compute_iterates(c(1,1), 5000)
cycle2 <- compute_iterates(c(1,3), 2000)
#cycle3 <- compute_iterates(c(2,26), 2000)
#cycle4 <- compute_iterates(c(3,27), 2000)

plot(cycle1, col = 'gray66')
points(cycle2, col='coral')
#points(cycle3, col='blue')
#points(cycle4, col='green')


table(shuriken[cycle1])

