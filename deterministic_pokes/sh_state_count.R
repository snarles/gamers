totals <- c()

for (max_count in 1:10) {
  grid0 <- 0:max_count
  lg <- length(grid0)
  
  dm <- do.call(cbind,
                lapply(0:5, function(k) rep(grid0, each=(lg^k), lg^(5-k))))
  
  count <- rowSums(dm)
  mins <- apply(dm, 1, min)
  
  totals[max_count] <- sum((mins == 0) & (count <= max_count))
}

totals
#[1]    7   28   84  210  462  923 1709 2975 4921 7798