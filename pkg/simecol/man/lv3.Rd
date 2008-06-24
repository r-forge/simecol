\name{lv3}
\docType{data}
\alias{lv3}
\title{Lotka-Volterra-Type Model with Resource, Prey and Predator}
\description{
  \pkg{simecol} example: predator prey-model with three equations:
  predator, prey and resource (e.g. nutriens, grassland).}

\usage{data(lv3)}

\format{A valid S4 object according to the \code{\link{odeModel}}
specification.  The object contains the following slots:

\describe{
  \item{\code{main}}{Lotka-Volterra equations for predator
    prey and resource}.
  \item{\code{parms}}{Vector with named parameters of the model:
    \describe{
      \item{\code{c}}{growth rate of the prey population,}
      \item{\code{d}}{encounter rate of predator and prey,}
      \item{\code{e}}{yield factor (allows conversion with respect to \code{d}),}
      \item{\code{f}}{death rate of the predator population,}
      \item{\code{g}}{recycling parameter.}
    }
  }
  \item{\code{inputs}}{Time series specifying external delivery of resource.}
  \item{\code{times}}{Simulation time and integration interval.}
  \item{\code{init}}{Vector with start values for \code{s}, \code{p} and \code{k}.
    \describe{
      \item{\code{s}}{Resource (e.g. grassland or phosphorus).}
      \item{\code{p}}{Producer (prey).}
      \item{\code{k}}{Consumer (predator).}
    }
  }
  \item{\code{solver}}{Character string specifying the integration method.}
}}

\seealso{
  \code{\link{simecol-package}},
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}

\examples{
    data(lv3)
    plot(sim(lv3))
    solver(lv3) <- "lsoda"
    times(lv3)["by"] <- 5  # set masximum external time step to a large value
    plot(sim(lv3))         # wrong! automatic time step overlooks internal inputs
    plot(sim(lv3, hmax=1)) # integration with correct maximum internal time step
}

\keyword{datasets}
