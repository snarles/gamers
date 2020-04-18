## Testing some features of sparse arrays

library(slam)
arr <- simple_sparse_zero_array(c(3, 3, 3, 4))
arr[2,2,1,1] <- 2
arr[2,3,1,4] <- 5
indmat <- rbind(c(2,2,1,1), c(2,3,1,4), c(1,1,1,2))
arr[indmat]
