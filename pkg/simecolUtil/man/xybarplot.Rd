\name{xybarplot}
\alias{xybarplot}

\title{Plot bars on irregular x locations}
\description{
 This function is an extension of \code{barplot} which plots bars on given
 locations on the x-Axes.
}
\usage{
xybarplot(x, y, wbox = min(diff(x)) * 0.9, ofs = 0.5, ...)
}

\arguments{
  \item{x}{vector of x locations}
  \item{y}{vector of y values (heights)}
  \item{wbox}{width of the boxes (currently only one single value allowed)}
  \item{ofs}{offset of the boxes}
  \item{\dots}{additional parameters of \code{barplot}}
}

\value{
 A numeric vector, giving the coordinates of the bar midpoints
  drawn, useful for adding to the graph.
}

\seealso{\code{\link{barplot}}, \code{\link{plot}}}
\examples{
x <- round((1:10)* 14 + runif(10, min=-5, max=5))
y <- rnorm(x, mean=50, sd=10)

xybarplot(x, y)
axis(1)
box()
}

\keyword{hplot}

