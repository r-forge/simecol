\name{seedfill}
\alias{seedfill}

\title{Color Fill Algorithm}
\description{
 Fills a bounded area within a numeric matrix with a given number (color).
}
\usage{
 seedfill(z, x=1, y=1, fcol=0, bcol=1, tol=1e-6)
}
\arguments{
  \item{z}{A matrix containing an image (double precision values are possible).}
  \item{x, y}{Start coordinates of the filled area.}
  \item{fcol}{Numeric value of the fill color.}
  \item{bcol}{Numeric value of the border value.}
  \item{tol}{Numeric value of border color tolerance.}
}
\seealso{
  \code{\link{neighbours}}
}
\details{
  The function implements a basic color fill algorithm for use in image
  manipulation or cellular automata.
}
\value{
 A matrix with the same structure as \code{z}.
}
\examples{
    # define a matrix
    z<-matrix(0, nrow=20, ncol=20)

    # draw some lines
    z[10,]<-z[,10] <- 1
    z[5,] <-z[,5]  <- 3

    # plot matrix and filled variants
    par(mfrow=c(2, 2))
    image(z)
    image(seedfill(z))
    image(seedfill(z ,x=15, y=15, fcol=1, bcol=3))
    image(seedfill(z, x=7, y=7, fcol=3, bcol=1))
}
\keyword{array}
