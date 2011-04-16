\name{eightneighbours}
\alias{eightneighbours}
\alias{eightneighbors}

\title{Count Number of Neighbours in a Rectangular Cellular Grid.}

\description{
  This function returns the sum of the eight neibours of a cell within a
  matrix. It can be used to simulate simple cellular automata,
  e.g. Conway's Game of Life.
}
\usage{
  eightneighbours(x)
  eightneighbors(x)
}
\arguments{
  \item{x}{The cellular grid, which typically contains integer values of
    zero (dead cell) or one (living cell).}
}

\value{
  A matrix with the same structure as \code{x}, but with the sum of the
  neighbouring cells of each cell.
}


\seealso{
  \code{\link{seedfill}}, \code{\link{neighbours}}, \code{\link{conway}}
}

\examples{
n <- 80; m <- 80
x <- matrix(rep(0, m*n), nrow = n)
x[round(runif(1500, 1, m*n))] <- 1
## uncomment this for another figure
#x[40, 20:60] <- 1

image(x, col=c("wheat", "grey", "red"))
x2 <- x
for (i in 2:10){
  nb <- eightneighbours(x)

  ## survive with 2 or 3 neighbours
  xsurv <- ifelse(x > 0 & (nb == 2 | nb ==3), 1, 0)

  ## generate for empty cells with 3 neigbours
  xgen <- ifelse(x == 0 & nb == 3, 1, 0)

  x  <- ((xgen + xsurv)>0)
  x2 <- ifelse(x2>1, 1, x2)
  x2 <- ifelse(x>0, 2, x2)

  #dim(x2) <-c(n,m)
  image(x2, col=c("wheat", "grey", "red"), add=TRUE)
}
}
\keyword{array}
