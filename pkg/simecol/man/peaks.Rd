\name{peaks}
\alias{peaks}
\title{Find Peaks Within xy-Data}
\description{
  The function returns maxima (values which have only smaller neighbours)
  and minima (values which have only larger neighbours).
}

\usage{peaks(x, y=NULL, mode="maxmin")}

\arguments{
  \item{x, y}{The coordinates of given points.}
  \item{mode}{Specifies if both maxima and minima (\code{mode="maxmin"})
  or only maxima (\code{mode="max"}) or minima (\code{mode="min"}) are
  requested.}
}

\value{
  A list with x and y coordinates of all peaks.
}

\seealso{
  \code{\link{approx}},
  \code{\link{upca}}
  
}
\examples{
    x <- sin(seq(0,10,0.1))
    plot(x)
    points(peaks(x), col="red", pch=15)
}

\keyword{misc}
