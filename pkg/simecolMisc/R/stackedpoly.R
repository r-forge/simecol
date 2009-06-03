stackedpoly <- function(x, columns, density = NA, angle = 45,
        border = NA, col = NA,  xlab = "x values", ylab = "y values", ...) {

  ## apply recycling rule to make all vectors of equal length
  m <- length(columns)
  density <- array(density, m)
  angle   <- array(angle, m)
  border  <- array(border, m)
  col     <- array(col, m)

  ## create x-data
  x1   <- x[[1]]
  xx <-  c(x1[1], x1, last(x1), x1[1])

  ## calculate y totals
  ysum <- rowSums(x[columns]) # sum of all selected rows

  ## draw empty plot
  plot(range(x1), range(0, ysum), type = "n", xlab = xlab, ylab = ylab, ...)
  ## draw polygons
  for (i in 1:m) {
    yy <- c(0,   ysum, 0      , 0)
    polygon(xx, yy, col = col[i], density = density[i],
      angle = angle[i], border = border[i], ...)
    ysum <- ysum - x[[columns[i]]]
  }
}
