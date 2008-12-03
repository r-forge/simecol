\name{fitOdeModel}
\alias{fitOdeModel}
\title{Parameter fitting for odeModel objects}
\description{
  Fit parameters of \code{odeModel} objects to measured data.
}
\usage{
fitOdeModel(simObj, whichpar = names(parms(simObj)), obstime, yobs, 
  sd.yobs = as.numeric(lapply(yobs, sd)), initialize = TRUE, 
  debuglevel = 0, fn = ssqOdeModel, 
  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "PORT"),
  lower = -Inf, 
  upper = Inf, 
  control = list(), ...)
}

\arguments{
  \item{simObj}{a valid object of class \code{\link{odeModel}}}
  \item{whichpar}{character vector with names of parameters which are to be 
  optimized (subset of parameter names of the \code{simObj}.}
  \item{obstime}{vector with time steps for which observational data are available}
  \item{yobs}{data frame with observational data for all or a subset of 
    state variables. Their names must correspond exacly with existing names of state 
    variables in the \code{\link{odeModel}}.}
  \item{sd.yobs}{vector of given standard deviations (or scale)
    for all observational variables given in \code{yobs}.
    If no standard deviations (resp. scales) are given,
    these are estimated from \code{yobs}.}
  \item{initialize}{optional boolean value whether the simObj should be 
    re-initialized after the assignment of new parameter values. This can be
    necessary in certain models to assign consistent values to initial state 
    variables if they depend on parameters.}
  \item{debuglevel}{a positive number that specifies the amount of debugging 
    information printed}
  \item{fn}{objective function, i.e. function that returns
    the quality criterium that is minimized, defaults to \code{\link{ssqOdeModel}}.}
  \item{method}{optimization method, see \code{\link[stats]{optim}}}
  \item{lower, upper}{Bounds of the parameters for method L-BFGS-B, see 
    \code{\link[stats]{optim}} and for method PORT see \code{\link[stats]{nlminb}}.
    The bounds are also respected by other optimizers
    by means of an internal transformation of the parameter space (see 
    \code{\link{p.constrain}}). In this case, \emph{named vectors} are required.}
  \item{control}{A list of control parameters for \code{\link[stats]{optim}}}
  \item{\dots}{additional parameters passed to the solver method
    (e.g. \code{\link[deSolve]{lsoda}})}
}
\details{
  This function works currently only with \code{odeModel} objects where 
  \code{parms} is a vector, not a list.
}
\value{
  A list with the optimized parameters and other information, 
  see \code{\link[stats]{optim}} resp. \code{\link[stats]{\code{\link{optim}}}} for details.
}
\note{
  The interface to the PORT algorithm is still under development and not yet functional!!!
}
\seealso{\code{\link{ssqOdeModel}}, \code{\link[stats]{optim}}}
\examples{
## ======== load example model =========
data(chemostat)

## derive scenarios
cs1 <- cs2 <- chemostat

## generate some noisy data
parms(cs1)[c("vm", "km")] <- c(2, 10)
times(cs1) <- c(from=0, to=20, by=2)
yobs <- out(sim(cs1))
obstime <- yobs$time
yobs$time <- NULL
yobs$S <- yobs$S + rnorm(yobs$S, sd= 0.1 * sd(yobs$S))*2
yobs$X <- yobs$X + rnorm(yobs$X, sd= 0.1 * sd(yobs$X))

## ======== optimize it! =========

## time steps for simulation, either small for rk4 fixed step
# times(cs2)["by"] <- 0.1
# solver(cs2) <- "rk4"

## or, faster: use lsoda and and return only required steps that are in the data
times(cs2) <- obstime
solver(cs2) <- "lsoda"

## Nelder-Mead (default)
whichpar  <- c("vm", "km")

res <- fitOdeModel(cs2, whichpar=whichpar, obstime, yobs,
  debuglevel=0,
  control=list(trace=TRUE))

## assign fitted parameters to the model, i.e. as start values for next step
parms(cs2)[whichpar] <- res$par

## alternatively, L-BFGS-B (allows lower and upper bounds for parameters)
res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs,
  debuglevel=0, fn = ssqOdeModel,
  method = "L-BFGS-B", lower = 0,
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

## alternative 2, transform parameters to constrain unconstrained method
## Note: lower and upper are *named* vectors
res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs,
  debuglevel=0, fn = ssqOdeModel,
  method = "BFGS", lower = c(vm=0, km=0), upper=c(vm=4, km=20),
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

## set model parameters to  fitted values and simulate again
parms(cs2)[whichpar] <- res$par
times(cs2) <- c(from=0, to=20, by=1)
ysim <- out(sim(cs2))

## plot results
par(mfrow=c(2,1))
plot(obstime, yobs$X, ylim = range(yobs$X, ysim$X))
lines(ysim$time, ysim$X, col="red")
plot(obstime, yobs$S, ylim= range(yobs$S, ysim$S))
lines(ysim$time, ysim$S, col="red")

}

\keyword{ misc }
