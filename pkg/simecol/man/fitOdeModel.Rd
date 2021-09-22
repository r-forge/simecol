\name{fitOdeModel}
\alias{fitOdeModel}
\title{Parameter Fitting for odeModel Objects}
\description{
  Fit parameters of \code{odeModel} objects to measured data.
}
\usage{
fitOdeModel(simObj, whichpar = names(parms(simObj)), obstime, yobs, 
  sd.yobs = as.numeric(lapply(yobs, sd)), initialize = TRUE, 
  weights = NULL, debuglevel = 0, fn = ssqOdeModel, 
  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "PORT",
   "newuoa", "bobyqa"),
  lower = -Inf, upper = Inf, scale.par = 1,
  control = list(), ...)
}

\arguments{
  
  \item{simObj}{a valid object of class \code{\link{odeModel}},}

  \item{whichpar}{character vector with names of parameters which are to
    be optimized (subset of parameter names of the \code{simObj}),}

  \item{obstime}{vector with time steps for which observational data are
    available,}

  \item{yobs}{data frame with observational data for all or a subset of
    state variables. Their names must correspond exacly with existing
    names of state variables in the \code{\link{odeModel}},}

  \item{sd.yobs}{vector of given standard deviations (or scale) for all
    observational variables given in \code{yobs}.  If no standard
    deviations (resp. scales) are given, these are estimated from
    \code{yobs},}

  \item{initialize}{optional boolean value whether the simObj should be
    re-initialized after the assignment of new parameter values. This
    can be necessary in certain models to assign consistent values to
    initial state variables if they depend on parameters.}
  
  \item{weights}{optional weights to be used in the fitting process.
    See cost function (currently only \code{\link{ssqOdeModel}}) for
    details.}
  
  \item{debuglevel}{a positive number that specifies the amount of
    debugging information printed,}

  \item{fn}{objective function, i.e. function that returns the quality
    criterium that is minimized, defaults to \code{\link{ssqOdeModel}},}

  \item{method}{optimization method, see \code{\link[stats]{nlminb}} for
    the PORT algorithm, \code{\link[minqa]{newuoa}} resp.
    \code{\link[minqa]{bobyqa}}  for the newuoa and bobyqa
    algorithms, and \code{\link[stats]{optim}} for all other  methods,}

  \item{lower, upper}{bounds of the parameters for method L-BFGS-B, see
    \code{\link[stats]{optim}}, PORT see \code{\link[stats]{nlminb}}
    and bobyqa \code{\link[minqa]{bobyqa}}.
    The bounds are also respected by other optimizers
    by means of an internal transformation of the parameter space (see 
    \code{\link{p.constrain}}). In this case, \emph{named vectors} are required.}

  \item{scale.par}{scaling of parameters for method PORT see
    \code{\link[stats]{nlminb}}.  In many cases, automatic scaling
    (\code{scale.par = 1}) does well, but sometimes (e.g. if parameter
    ranges differ several orders of magnitude) manual adjustment is
    required. Often you get a reasonable choice if you set
    \code{scale.par = 1/upper}.  The parameter is ignored by all other
    methods. For \code{"Nelder-Mead"}, \code{"BFGS"}, \code{"CG"} and
    \code{"SANN"} parameter scaling occurs as a side effect of parameter
    transformation with \code{p.constrain}.}

  \item{control}{a list of control parameters for
    \code{\link[stats]{optim}} resp.  \code{\link[stats]{nlminb}},}
  
  \item{\dots}{additional parameters passed to the solver method
    (e.g. to \code{\link[deSolve]{lsoda}}).}
  
}
\details{
  This function works currently only with \code{odeModel} objects where
  \code{parms} is a vector, not a list.

  Note also that the control parameters of the PORT algorithm are
  different from the control parameters of the other optimizers.
}
\value{
  A list with the optimized parameters and other information, see
  \code{\link[stats]{optim}} resp. \code{\link[stats]{nlminb}} for
  details.
}
\references{
 Gay, D. M. (1990) Usage Summary for Selected Optimization Routines.
 Computing Science Technical Report No. 153. AT&T Bell Laboratories,
 Murray Hill, NJ.

 Powell, M. J. D. (2009). The BOBYQA algorithm for bound constrained
 optimization without derivatives. Report No. DAMTP 2009/NA06, Centre
 for Mathematical Sciences, University of Cambridge, UK.
 \url{https://www.damtp.cam.ac.uk/user/na/NA_papers/NA2009_06.pdf}
}
\seealso{

  \code{\link{ssqOdeModel}}, \code{\link{optim}}, \code{\link[stats]{nlminb}},
    \code{\link[minqa]{bobyqa}}

  Note also that package \pkg{FME} function
  \code{\link[FME:modFit]{modFit}} has even more flexible means to fit
  model parameters.

  Examples are given in the package vignettes.
}
\examples{

## ======== load example model =========
data(chemostat)

#source("chemostat.R")

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

coef(res)

## assign fitted parameters to the model, i.e. as start values for next step
parms(cs2)[whichpar] <- coef(res)

## alternatively, L-BFGS-B (allows lower and upper bounds for parameters)
res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs,
  debuglevel=0, fn = ssqOdeModel,
  method = "L-BFGS-B", lower = 0,
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

coef(res)

## alternative 2, transform parameters to constrain unconstrained method
## Note: lower and upper are *named* vectors
res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs,
  debuglevel=0, fn = ssqOdeModel,
  method = "BFGS", lower = c(vm=0, km=0), upper=c(vm=4, km=20),
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

coef(res)


## alternative 3a, use PORT algorithm
parms(cs2)[whichpar] <- c(vm=1, km=2)

lower <- c(vm=0, km=0)
upper <- c(vm=4, km=20)

res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs,
  debuglevel=0, fn = ssqOdeModel,
  method = "PORT", lower = lower, upper = upper,
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

coef(res)

## alternative 3b, PORT algorithm with manual parameter scaling
res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs,
  debuglevel=0, fn = ssqOdeModel,
  method = "PORT", lower = lower, upper = upper, scale.par = 1/upper,
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

coef(res)

## set model parameters to  fitted values and simulate again
parms(cs2)[whichpar] <- coef(res)
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
