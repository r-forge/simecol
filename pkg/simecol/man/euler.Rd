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
}
\arguments{
  \item{y }{the initial values for the ode system.  If \code{y} has a
      name attribute, the names will be used to label the output matrix.}
  \item{times }{times at which explicit estimates for \code{y} are
      desired.  The first value in \code{times} must be the initial time.}
  \item{func }{a user-supplied function that computes the values of the
      derivatives in the ode system (the \emph{model defininition}) at time
      t.
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
  \item{... }{additional arguments, allowing this to be a generic function}

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
  \code{\link[odesolve]{rk4}}, \code{\link[odesolve]{lsoda}}, \code{\link{iteration}}.
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

