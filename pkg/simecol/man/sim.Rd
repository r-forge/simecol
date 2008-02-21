\name{sim}
\alias{sim}
\alias{sim-methods}
\alias{sim,simObj-method}
\alias{sim,odeModel-method}
\alias{sim,gridModel-method}
\title{Simulation of 'simObj' model objects}
\description{
 This function provides the core functionality of the `simecol' package.
 Several methods depending on the class of the model are available.
}
\usage{
  sim(obj, ...)
  # sim(obj, animation=FALSE, delay=0, ...)
}
\arguments{
  \item{obj}{an object of class \code{simObj} or one of its subclasses.}
  \item{animation}{logical value to switch animation on (for classes
    \code{gridModel} and \code{rwalkModel}.}
  \item{delay}{delay (in ms and in addition to the time
    needed for the simulation) between consecutive images (for \code{gridModel}s)
    or xy-plots (for \code{rwalkModel}s).}
   \item{...}{optional parameters passed to the solver function
     (e.g. \code{hmax} for \code{lsoda}).}
}
\section{Methods}{
  \describe{
  \item{obj = "simObj"}{simulates the respective model object with
    optional animation.}
  \item{obj = "odeModel"}{simulates the respective model object.}
  \item{obj = "gridModel"}{simulates the respective model object with
    optional animation.}
  }
}
\details{
  Objects of class \code{rwalkModel} are simulated by the default \code{simObj}
  method. If you derive own sublasses from \code{simObj} it may be neccessary
  to write an appropriate \code{sim} method and/or solver function.
}
\value{
  The function returns the complete \code{simObj} instance with the
  simulation results in the \code{out} slot.
}

\examples{
    data(lv)
    plot(sim(lv))

    lv2 <- lv
    parms(lv2)["k1"] <- 0.5
    lv2 <- sim(lv2)
    plot(out(lv2))
}
\keyword{methods}
\keyword{programming}
