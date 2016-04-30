####
##  Order 2..
####

p <- 7 # base dimension
nms1 <- paste(0:(p-1))
nms2 <- c()
for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    nms2 <- c(nms2, paste(nms1[i], nms1[j], sep = "."))
  }
}

nms <- c(nms1, nms2)
names(nms) <- paste0("c", 0:(length(nms)-1))

nms

(lastind <- (p-1) + p * (p-1)/2)


j <- 3
str <- paste(p - j, p - j + 1, sep = ".")
nms[nms==str]
lastind - j*(j -1)/2 + 1


x <- 1; y <- 5
nms[nms == paste(x, y, sep = ".")]
index2(p, x, y)

