\name{iteration}
\alias{iteration}
\alias{iteration-methods}
\alias{iteration,simObj-method}
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
  \item{y}{The \code{simObj} object to be iterated.}
  \item{times}{Placeholder for compatibility with the \pkg{odesolve} package.}
  \item{func}{Placeholder for compatibility with the \pkg{odesolve} package.}
  \item{parms}{Placeholder for compatibility with the \pkg{odesolve} package.}
  \item{animate}{Animation during the simulation (if available for the specified
    class.}
  \item{\dots}{Optional arguments passed to the \code{plot} function if
    \code{animate=TRUE}.}
}

\value{A list of the model outputs (states \dots) for each timestep.}

\details{
  The solver method \code{iteration} is used to simulate discrete event
  models. It creates a local environment in a way that parameters,
  inputs and equations are visible within the main function of the
  \code{simObj}.
  
  In contrast to the ODE solvers, the \code{main} function of the model
  must not return the first derivative but instead of this
  the new state at the specified times explicitly.
  
  The actual value of time is available in the \code{main} function as \code{time}
  and the current increment as \code{parms$DELTAT} or \code{parms["DELTAT"]}
  depending on the data type of \code{parms}.
  
  Normally, this function is run indirectly from \code{\link{sim}}.
}
\seealso{
  \code{\link{sim}}, \code{\link{parms}},
  \code{\link[odesolve]{lsoda}}, \code{\link{rk4}}, \code{\link{euler}}.
}

\examples{
    data(conway)
    ## plot after simulation:
    plot(sim(conway), delay=100)

    ## plot during simulation
    sim(conway, animate=TRUE, delay=100)
}
\keyword{misc}
