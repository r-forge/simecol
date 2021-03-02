\name{pcuseries}
\alias{pcuseries}
\alias{pcu}
\alias{rho2alpha}
\alias{alpha2rho}
\title{Generate Plackett Bivariate Random Numbers}
\description{
  Generate bivariate uniform random numbers according to the Plackett
  distribution.
}
\usage{
pcu(x, alpha = rho2alpha(rho), rho)
pcuseries(n, alpha = rho2alpha(rho), rho, min = 0, max = 1)
alpha2rho(alpha)
rho2alpha(rho)
}
\arguments{
  \item{n}{number of observations.}
  \item{x}{vector of uniformly [0, 1] distributed real numbers.}
  \item{alpha}{association coefficient of the Plackett distribution.}
  \item{rho}{Pearson correlation coefficient.}
  \item{min, max}{lower and upper limits of the distribution. Must be
    finite.
  }
}
\details{
  The functions can be used to generate bivariate distributions with
  uniform marginals. Function \code{pcu} generates a vector of uniform
  random values of \code{length(x)} which are correlated to the
  corresponding vector \code{x}, \code{pcuseries} generates an
  auto-correlated series, and \code{alpha2rho} resp. \code{rho2alpha}
  convert between the Pearson correlation coefficient and the association
  measure of the Plackett distribution.
}
\references{ 
  Johnson, M., Wang, C., & Ramberg, J. (1984). Generation of
  multivariate distributions for statistical
  applications. \emph{American Journal of Mathematical and Management
  Sciences}, \bold{4}, 225-248.

  Nelsen, R. B. (2006). \emph{An Introduction to Copulas}. Springer, New
  York.
}
\seealso{\code{\link{runif}}}
\examples{
x <- runif(100)
y <- pcu(x, rho = 0.8)
plot(x, y)
cor(x, y)

x <- pcuseries(1000, rho=0.8)
plot(x, type="l")
acf(x)
pacf(x)
}

\keyword{distribution}

