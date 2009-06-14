laggedMatrix <- function(x, k) {
  L <- length(x+k)
  z <- matrix(NA, nrow = L + k - 1, ncol = k)
  for (i in 1:k) {
    z[(1:L) + (i - 1), i] <- x
  }
  return(z)
}
