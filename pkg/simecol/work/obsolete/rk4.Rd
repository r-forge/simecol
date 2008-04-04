\name{rk4}
\alias{rk4}
\alias{rk4-methods}
\alias{rk4,numeric-method}
\alias{rk4,odeModel-method}


\title{Runge-Kutta 4th order integration}
\description{
  Solving initial value problems for 
  systems of first-order ordinary differential equations
  (ODEs) using the classical Runge-Kutta 4th order integration. This is
  a special version designed to work with \code{odeModel} objects.
}
\usage{
  rk4(y, times, func, parms, ...) 
  # rk4(y) 
}
\arguments{
  \item{y}{The \code{odeModel} object to be solved.}
  \item{times}{Placeholder for compatibility with the odesolve package}
  \item{func}{Placeholder for compatibility with the odesolve package}
  \item{parms}{Placeholder for compatibility with the odesolve package}
  \item{...}{Additional parameters, reserved for extensions}
}

\value{A list of the model outputs (states \dots) for all timesteps.}

\details{
  The solver method \code{rk4} is used to simulate ODE 
  models. It creates a local environment in a way that parameters,
  inputs and equations are visible within the main function of the
  \code{odeModel}.
  Normally, this function is called indirectly from \code{\link{sim}}.
}

\seealso{
  \code{\link{sim}},
  \code{\link{parms}}
  \code{\link[odesolve]{lsoda}},
  \code{\link{euler}},
  \code{\link{iteration}},
  original \code{\link[odesolve]{rk4}} from the \code{odesolve} package.
}

\examples{
    # ordinary usage
    data(lv)
    solver(lv) <- "rk4"
    plot(sim(lv))

    # alternative usage
    rk4(lv)
}
\keyword{misc}
