\name{euler}
\alias{euler}
\alias{euler-methods}
\alias{euler,numeric-method}
\alias{euler,odeModel-method}


\title{Euler Integration}
\description{
  Solving initial value problems for 
  systems of first-order ordinary differential equations
  (ODEs) using the simple Euler forward integration method.
  A special version is available to work with \code{odeModel} objects.
}
\usage{
  euler(y, times, func, parms, ...) 
  # euler(y) 
}
\arguments{
  \item{y}{The \code{odeModel} object to be solved.}
  \item{times}{Placeholder for compatibility with the \pkg{odesolve} package.}
  \item{func}{Placeholder for compatibility with the \pkg{odesolve} package.}
  \item{parms}{Placeholder for compatibility with the \pkg{odesolve} package.}
  \item{...}{Reserved for further extensions.}

}

\value{A data frame of the model outputs (e.g. states) for all timesteps.}

\details{
  The solver method \code{euler} is used to simulate ODE 
  models. It creates a local environment in a way that parameters,
  inputs and equations are visible within the main function of the
  \code{odeModel}.
  Normally, this function is called indirectly from \code{\link{sim}}.
  The Euler method is the most primitive integration method and can be regarded
  simply as a fixed step solution of the corresponding difference equation.
}

\seealso{
  \code{\link{sim}}, \code{\link{parms}}
  \code{\link{rk4}}, \code{\link[odesolve]{lsoda}}, \code{\link{iteration}}.
}

\examples{
    # ordinary usage
    data(lv)
    solver(lv) <- "euler"
    plot(sim(lv))

    # alternative usage
    euler(lv)
}
\keyword{misc}

