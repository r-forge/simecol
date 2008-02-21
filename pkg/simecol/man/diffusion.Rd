\name{diffusion}
\docType{data}
\alias{diffusion}
\title{A Random Walk Particle Diffusion Model}
\description{
  \pkg{simecol} example: This is a random walk (basic particle
  diffusion) model.}

\usage{data(diffusion)}

\format{An S4 object according to the \code{\link{rwalkModel}} specification. 
The object contains the following slots:

\describe{
  \item{\code{main}}{A function with the movement rules for the particles.}
  \item{\code{parms}}{A list with the following components:
    \describe{
      \item{\code{ninds}}{number of simulated particles,}
      \item{\code{speed}}{speed of the particles,}
      \item{\code{area}}{vector with 4 elements giving the coordinates (left,
      bottom, right, top) of the coordinate system.}
    }}
  \item{\code{times}}{Simulation time (discrete time steps, \code{by}-argument
    ignored).}
  \item{\code{init}}{Data frame holding the start properties (Cartesian
    coordinates \code{x} and \code{y} and movement angle \code{a}) of the
    particles.}
}
}

\details{
  To see all details, please have a look into the implementation.
}

\seealso{
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
  \dontrun{
    data(diffusion)
    plot(sim(diffusion))
    # show "grid of environmental conditions"
    image(inputs(diffusion))
    # set conditions to "1", i.e. remove "refuge" in the middle
    inputs(diffusion) <- matrix(1, 100, 100)
    plot(sim(diffusion))
  }
}

\keyword{datasets}
