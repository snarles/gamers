####
##  Order 3..
####

p <- 7 # base dimension
nms1 <- paste(0:(p-1))
nms2 <- c()
for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    nms2 <- c(nms2, paste(nms1[i], nms1[j], sep = "."))
  }
}
nms3 <- c()
for (i in 1:(p-2)) {
  for (j in (i+1):(p-1)) {
    for (k in (j+1):p) {
      nms3 <- c(nms3, paste(nms1[i], nms1[j], nms1[k], sep = "."))
    }
  }
}

nms <- c(nms1, nms2, nms3)
names(nms) <- paste0("c", 0:(length(nms)-1))

nms

(lastind <- (p-1) + p * (p-1)/2 + p * (p - 1) * (p - 2)/6)
x = 1; y = 3; z = 5
j = p - x
(lastind2 <- lastind - j * (j-1) * (j-2)/6 + (j-1) * (j-2)/2)
k = p - y
(subind <- lastind2 - k * (k-1)/2 + 1)
subind + z - (y + 1)

lastind + 1 - j*(j-1)*(j-2)/6 + (j-1)*(j-2)/2 - k*(k-1)/2 + z-y-1

nms[nms==paste(x, y, z, sep = ".")]
