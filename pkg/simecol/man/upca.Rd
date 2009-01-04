\name{upca}
\docType{data}
\alias{upca}
\title{The Uniform Period Chaotic Amplitude Model}
\description{
  \pkg{simecol} example: resource-predator-prey model, which is able to exhibit
   chaotic behaviour.
}
\usage{
  data(upca)
}
\format{S4 object according to the \code{\link{odeModel}} specification.
The object contains the following slots:
  \describe{
    \item{\code{main}}{The differential equations for predator
    prey and resource with:}
    \describe{
      \item{\code{u}}{resource (e.g. grassland or phosphorus),}
      \item{\code{v}}{producer (prey),}
      \item{\code{w}}{consumer (predator).}
    }
    \item{\code{equations}}{Two alternative (and switchable) equations
      for the functional response.}
    \item{\code{parms}}{Vector with the named parameters of the model,
      see references for details.}
    \item{\code{times}}{Simulation time and integration interval.}
    \item{\code{init}}{Vector with start values for \code{u}, \code{v} and \code{w}.}
    \item{\code{solver}}{Character string with the integration method.}
  }
}

\details{
  To see all details, please have a look into the implementation and the
  original publications.
}
\references{
  Blasius, B., Huppert, A., and Stone, L. (1999) Complex dynamics and
  phase synchronization in spatially extended ecological systems.
  \emph{Nature}, \bold{399} 354--359.

  Blasius, B. and Stone, L. (2000) Chaos and phase synchronization in
  ecological systems.  \emph{International Journal of Bifurcation and
  Chaos}, \bold{10} 2361--2380.
}

\seealso{
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}
\examples{
    data(upca)
    plot(sim(upca))

    # omit stabilizing parameter wstar
    parms(upca)["wstar"] <- 0
    plot(sim(upca))

    # change functional response from
    # Holling II (default) to Lotka-Volterra
    equations(upca)$f <- function(x, y, k) x * y
    plot(sim(upca))
}

\keyword{datasets}
