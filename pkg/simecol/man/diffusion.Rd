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
    ## (1) basic example
    plot(sim(diffusion))
    ## show "grid of environmental conditions"
    image(inputs(diffusion))
    
    ## (2) scenario
    ##     with homogeneous environment (no "refuge" in the middle)
    no_refuge <- diffusion # Cloning of the whole model object
    inputs(no_refuge) <- matrix(1, 100, 100)
    plot(sim(no_refuge))
    
    ## (3) example of using an observer function
    observer(diffusion) <- function(state, ...) {
      ## numerical output to the screen
      cat("mean x=", mean(state$x),
          ", mean y=", mean(state$y),
          ", sd   x=", sd(state$x),
          ", sd   y=", sd(state$y), "\n")
      # animation
      par(mfrow=c(2,2))
      plot(state$x, state$y, xlab="x", ylab="y", pch=16, col="red", xlim=c(0, 100))
      hist(state$y)
      hist(state$x)
      # default case: return the state --> iteration stores it in "out"
      init
    }

    sim(diffusion)
    
    ## (4) remove the observer
    observer(diffusion) <- NULL
    diffusion <- sim(diffusion)
    
  }
}

\keyword{datasets}
