\name{laggedMatrix}
\alias{laggedMatrix}
\title{Create a matrix with lagged columns}
\description{
  Create a matrix with lagged columns.
}
\usage{
laggedMatrix(x, k)
}

\arguments{
  \item{x}{a vector}
  \item{k}{number of columns, with lag = 0:(k-1).}
}

\value{
  a matrix with \code{k + length(x) -1} rows and \code{k} columns.
}

\note{

  This is a rather special function. Use \code{\link[stats]{acf}} for
  the standard way to estimate autocorrelations and
  \code{\link[stats]{arima}} for fitting time-series models.
  
}

\seealso{\code{\link[stats]{acf}}, \code{\link[stats]{arima}}}
\examples{
x <- sort(runif(100))
xx <- laggedMatrix(x, 3)
matplot(xx)
cor(na.omit(xx))

cor(x[-1], x[-length(x)])

## but note:
print(acf(x, lag=3))
}
\keyword{ misc }
