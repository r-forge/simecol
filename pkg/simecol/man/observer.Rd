\name{observer}

\alias{observer}
\alias{observer-methods}
\alias{observer,simObj-method}

\alias{observer<-}
\alias{observer<--methods}
\alias{observer<-,simObj-method}

\docType{methods}

\title{Get or Set an Observer Functions to an `simObj' Object}
\description{
  Get or set a user-defined observer to enable user-specified storage
  of simulation results, visualisation or logging.
}
\usage{

observer(obj, ...)
observer(obj) <- value
}
\arguments{
  \item{obj}{A valid \code{simObj} instance.}
  \item{value}{A function specifying an observer, see Details.
  }
  \item{...}{Reserved for method consistency.}
}

\details{
  The observer can be used with solver \code{\link{iteration}} or a user-defined
  solver function. It does not work with differential equations solvers.
  
  The observer is a function with the following arguments:
  
  \code{function(state)}
  
  or:
  
  \code{function(state, time, i, out, y)}
  
  Where \code{state} is the actual state of the system, \code{time} and \code{i}
  are the simulation time and the indexof the time step respectively,
  \code{out} is the output of the actual simulation collected so far. 
  The original object used in the simulation is passed via \code{y} and can be used
  to get access on parameter values or model equations.
  
  If available, the observer function is called for every time step in the 
  iteration. It can be used for calculations ``on the fly'' to reduce memory
  of saved data, for user-specified animation or for logging purposes.
  
  If the value returned by observer is a vector, than resulting \code{out} will be a
  \code{data.frame}, otherwise it will be a list of all states.
 
}

\value{
  The observer function either modifies \code{obj} or it returns the 
  assigned observer function or \code{NULL} (the default).
}

\seealso{
  \code{\link{iteration}} for the iteration solver,
  
  \code{\link{parms}} for accessor and replacement functions of other slots,
  
  \code{\link{simecol-package}} for an overview of the package.
}
\examples{

## load model "diffusion"
data(diffusion)

solver(diffusion) # solver is iteration, supports observer

### == Example 1 ===============================================================

## assign an observer for visualisation
observer(diffusion) <- function(state) {
  ## numerical output to the screen
  cat("mean x=", mean(state$x),
      ", mean y=", mean(state$y),
      ", sd   x=", sd(state$x),
      ", sd   y=", sd(state$y), "\n")
  ## animation
  par(mfrow = c(2, 2))
  plot(state$x, state$y, xlab = "x", ylab = "y", pch = 16, 
    col = "red", xlim = c(0, 100))
  hist(state$y)
  hist(state$x)
  
  ## default case: 
  ## return the state --> iteration stores full state in "out"
  state
}

sim(diffusion)

### == Example 2 ===============================================================

## an extended observer with full argument list
observer(diffusion) <- function(state, time, i, out, y) {
  ## numerical output to the screen
  cat("index =", i,
      ", time =", time,
      ", sd   x=", sd(state$x),
      ", sd   y=", sd(state$y), "\n")
  ## animation
  par(mfrow = c(2, 2))
  plot(state$x, state$y, xlab = "x", ylab = "y", pch = 16, 
    col = "red", xlim = c(0, 100))
  hist(state$y)
  hist(state$x)
  if (is.matrix(out)) # important because out may be NULL for the first call
    matplot(out[,1], out[,-1]) # dynamic graph of sd in both directions
  
  ## return a vector with summary information
  c(times = time, sdx=sd(state$x), sdy=sd(state$y))
}

diffusion <- sim(diffusion)

### == Restore default =========================================================

observer(diffusion) <- NULL # delete observer
diffusion <- sim(diffusion)

}
\keyword{programming}
