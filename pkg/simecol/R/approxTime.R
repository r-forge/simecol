## special versions of approx:
##  approxTime:  interpolation of complete rows of a matrix or data frame
##  approxTime1: special case with one row only (slightly faster)

approxTime <- function(x, xout, ...) {
  if (is.data.frame(x)) {x <- as.matrix(x); wasdf <- TRUE} else wasdf <- FALSE
  if (!is.matrix(x)) stop("x must be a matrix or data frame")
  m <- ncol(x)
  y <- matrix(0, nrow=length(xout), ncol=m)
  y[,1] <- xout
  for (i in 2:m) {
    y[,i] <- as.vector(approx(x[,1], x[,i], xout, ...)$y)
  }
  if (wasdf) y <- as.data.frame(y)
  names(y) <- dimnames(x)[[2]]
  y
}

approxTime1 <- function(x, xout, rule=1) {
  # first colum:   x (independend values, e.g. time)
  # other columns: y (dependend data)
  # ... error checking for dataframe or matrix etc ...
  if (!is.matrix(x)) x <- as.matrix(x)
  n <- nrow(x)
  if (xout >= x[n,1]) {
    y <- x[n,]
    if (rule==1 & (xout > x[n+1])) y[2:length(y)] <- NA
  } else if (xout <= x[1,1]) {
      y <- x[1,]
      if (rule==1 & (xout < x[1])) y[2:length(y)] <- NA
  } else {
      i  <- which.max(x[,1] > xout)
      x1 <- x[i-1,1]
      x2 <- x[i,1]
      y1 <- x[i-1,]
      y2 <- x[i,]
      y  <- y1 + (y2 - y1) * (xout - x1) / (x2 - x1)
  }
  names(y) <-  dimnames(x)[[2]]
  y
}
