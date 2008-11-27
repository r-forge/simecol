\name{p.constrain}
\alias{p.constrain}
\alias{p.unconstrain}

\title{Transform data between unconstrained and box box-constrained scale}
\description{
  These functions can be used to transform a vector of data or parameters
  between unconstrained [-Inf, Inf] and box-constrained representation 
  (interval [lower, upper]).
}
\usage{
p.constrain(p, lower = -Inf, upper = Inf, f = 1)
p.unconstrain(p, lower = -Inf, upper = Inf, f = 1)
}

\arguments{
  \item{p}{vector of data (e.g. model parameters),}
  \item{lower, upper}{vectors with lower resp. upper bounds used for 
    transformation,}
  \item{f}{optional scaling factor.}
}
\details{
  These functions are employed by \code{\link{fitOdeModel}} \code{\link{ssqOdeModel}}
  in order to be able to use the unconstrained optimizers of \code{\link{optim}} for
  constrained optimization. 
  The transformation functions are
  \deqn{p' = \tan(\pi/2 \cdot (2 p - upper - lower) / (upper - lower)) \cdot 1/f}{%
       p' = tan(pi/2 * (2 * p - upper - lower) / (upper - lower)) / f}  
   and its inverse
   \deqn{p = (upper + lower)/2 + (upper - lower) \cdot \arctan(f \cdot p')/\pi}{%
        p = 1/2 * (upper + lower) + (upper - lower) * arctan(f * p')/pi}.
}
\value{
  vector with transformed (resp. back-transformed) values.
}
\references{Reichert, T. (1998) AQUASIM 2.0 User Manual.
Computer Program for the Identification and Simulation
of Aquatic Systems. Swiss Federal Institute for Environmental Science 
and Technology (EAWAG), CH - 8600 Duebendorf Switzerland,
\url{http://www.aquasim.eawag.ch/e_aquasim_manual.html}}


\seealso{\code{\link{fitOdeModel}}, \code{\link{ssqOdeModel}}}

\examples{
xx <- seq(-100, 100, 2)
plot(xx, yy<-p.constrain(xx, -20, 45), type="l")
points(p.unconstrain(yy, -20, 45), yy, col="red")
}
\keyword{ misc }

