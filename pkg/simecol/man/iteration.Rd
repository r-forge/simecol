\name{iteration}
\alias{iteration}
\alias{iteration-methods}
\alias{iteration,numeric-method}
\alias{iteration,simObj-method}
\alias{iteration,gridModel-method}
\alias{iteration,odeModel-method}

\title{Discrete Simulation}
\description{
  Solver function to simulate discrete ecological (or other) dynamic
  models. It is normally called indirectly from \code{sim}.
}
\usage{
  iteration(y, times=FALSE, func=FALSE, parms=FALSE, animate = FALSE, ...)
}
\arguments{
  \item{y }{the initial values for the system.  If \code{y} has a
      name attribute, the names will be used to label the output matrix.}
  \item{times }{times at which explicit estimates for \code{y} are
      desired.  The first value in \code{times} must be the initial time.}
  \item{func }{a user-supplied function that computes the values of the
      \emph{next time step} (not the derivatives !!!) 
      in the system (the \emph{model defininition}) at time t.
      The user-supplied function  \code{func} must be called as:
      \code{yprime = func(t, y, parms)}.  \code{t} is the current time point
      in the integration, \code{y} is the current estimate of the variables
      in the ode system, and \code{parms} is a vector of parameters (which
      may have a names attribute, desirable in a large system).
  
      The return value of func should be a list, whose first element is a
      vector containing the derivatives of \code{y} with respect to
      \code{time}, and whose second element is a vector (possibly with a
      \code{\link{names}} attribute) of global values that are required at
      each point in \code{times}.}
  \item{parms }{vector or list holding the parameters used in \code{func} 
     that should be modifiable without rewriting the function.}
  \item{animate}{Animation during the simulation (if available for the specified
    class.}
  \item{\dots}{Optional arguments passed to the \code{plot} function if
    \code{animate=TRUE}.}
}

\value{A list of the model outputs (states \dots) for each timestep.}

\details{
  The solver method \code{iteration} is used to simulate discrete event
  models. 
  
  In contrast to differential equation solvers, the \code{main} function of the model
  must not return the first derivative but instead and explicitly
  the new state at the specified times.
  
  The actual value of time is available in the \code{main} function as \code{time}
  and the current increment as \code{parms$DELTAT} or \code{parms["DELTAT"]}
  depending on the data type of \code{parms}.
  
  Normally, this function is run indirectly from \code{\link{sim}}.

  The default \code{iteration} method of class \code{simObj} supports the
  \code{observer} mechanism. This means that a function stored in slot
  \code{observer} is called during each iteration step with the return value
  of \code{main} as its first argument. You can use this to determine the amount
  of data stored during each iteration step (e.g. whole population or only
  mean values for individual based models), to do run-time animation or to
  write log files.
}

\seealso{
  \code{\link{sim}}, \code{\link{parms}},
  \code{\link[deSolve]{lsoda}}, \code{\link[deSolve]{rk4}}, \code{\link[deSolve]{euler}}.
}

\examples{
data(conway)
## plot after simulation:
plot(sim(conway), delay=100)

## plot during simulation
sim(conway, animate=TRUE, delay=100)

##### discrete version of logistic growth equation
dlogist <- new("odeModel",
main = function (time, init, parms, ...) {
  x <- init
  p <- parms
  x[1] <- x[1] + p["r"] * x[1] * (1 - x[1] / p["K"])
  #       ^^^^ important !!! new value, not derivative
  list(c(x))
},
parms  = c(r=0.1, K=10),
times  = seq(0, 100, 1),
init   = c(population=0.1),
solver = "iteration" #!!!
)

plot(sim(dlogist))

parms(dlogist)["r"] <- 2
plot(sim(dlogist))
}
\keyword{misc}
