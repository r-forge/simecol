\name{ssqOdeModel}
\alias{ssqOdeModel}

\title{Sum of Squares Between odeModel and Data}
\description{
  Compute the sum of squares between a given data and an \code{odeModel}
  object.
}
\usage{
ssqOdeModel(p, simObj, obstime, yobs, 
  sd.yobs = as.numeric(lapply(yobs, sd)), 
  initialize = TRUE, lower. = -Inf, upper. = Inf,
  debuglevel = 0, ..., pnames = NULL)
}

\arguments{

  \item{p}{vector of named parameter values of the model (can be a
    subset),}
  
  \item{simObj}{a valid object of class \code{\link{odeModel}},}

  \item{obstime}{vector with time steps for which observational data are
    available,}

  \item{yobs}{data frame with observational data for all or a subset of
    state variables. Their names must correspond exacly with existing
    names of state variables in the \code{\link{odeModel}}.}
  
  \item{sd.yobs}{vector of given standard deviations for all
    observational variables given in \code{yobs}. If no standard
    deviations are given, these are estimated from yobs.}
  
  \item{initialize}{optional boolean value whether the \code{simObj}
    should be re-initialized after the assignment of new parameter
    values. This can be necessary in certain models to assign consistent
    values to initial state variables if they depend on parameters.}
  
  \item{lower., upper.}{named vectors with lower and upper bounds used
    in the optimisation,}
  
  \item{debuglevel}{a positive number that specifies the amount of
    debugging information printed,}
  
  \item{\dots}{additional parameters passed to the solver method (e.g.
    \code{\link[deSolve]{lsoda}}),}

  \item{pnames}{names of the parameters, optionally passed from
    fitOdeModel. This argument is a workaround for \R versions below
    2.8.1. It may be removed in future versions of \pkg{simecol}.}
}
\details{
  This is the default function called by function
  \code{\link{fitOdeModel}}. The source code of this function can be
  used as a starting point to develop user-defined optimization
  criteria (cost functions).
  
}
\value{
  The sum of squared differences between \code{yobs} and simulation,
  weighted by the inverse of the standard deviations of the respective
  variables.
}

\seealso{
  \code{\link{fitOdeModel}}, \code{\link{optim}},
  \code{\link{p.constrain}}
}

\examples{
data(chemostat)
cs1 <- chemostat

## generate some noisy data
parms(cs1)[c("vm", "km")] <- c(2, 10)
times(cs1) <- c(from=0, to=20, by=2)
yobs <- out(sim(cs1))
obstime <- yobs$time
yobs$time <- NULL
yobs$S <- yobs$S + rnorm(yobs$S, sd= 0.1 * sd(yobs$S))*2
yobs$X <- yobs$X + rnorm(yobs$X, sd= 0.1 * sd(yobs$X))

## SSQ between model and data
ssqOdeModel(NULL, cs1, obstime, yobs)

## SSQ between model and data, different parameter set
ssqOdeModel(p=c(vm=1, km=2), cs1, obstime, yobs)
}
\keyword{ misc }

