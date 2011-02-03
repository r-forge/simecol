\name{chemostat}
\docType{data}
\alias{chemostat}
\title{Chemostat Model}
\description{
  \pkg{simecol} example: Model of continuos culture of microorganisms
  (chemostat).
}
\usage{data(chemostat)}

\format{

  An S4 object according to the \code{\link{odeModel}} specification.
  The object contains the following slots:

\describe{
  \item{\code{main}}{the differential equations for substrate (\code{S})
    and cells (\code{X}).
  }
  \item{\code{parms}}{a vector with the named parameters of the model:
    \describe{
      \item{\code{vm}}{maximum growth rate of the cells,}
      \item{\code{km}}{half saturation constant,}
      \item{\code{Y}}{yield coefficient (conversion factor of substrate
	into cells).
      }
      \item{\code{D}}{dilution rate,}
      \item{\code{S0}}{substrate concentration in the inflow.}          
    }
  }
  \item{\code{times}}{simulation time and integration interval.}
  \item{\code{init}}{vector with start values for substrate (\code{S})
    and cells (\code{X}).
  }
} 
To see all details, please have a look into the implementation below.
}


\seealso{
  \code{\link{simecol-package}},
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}



\examples{
##============================================
## Basic Usage:
##   work with the example
##============================================
data(chemostat)
plot(sim(chemostat))

parms(chemostat)["D"] <- 0.9 
plot(sim(chemostat))



##============================================
## Implementation:
##   The code of the chemostat model
##============================================
chemostat <- new("odeModel",
  main = function(time, init, parms, inputs = NULL) {
    with(as.list(c(init, parms)), {
      mu  <- vm * S/(km + S)              # Monod equation
      dx1 <- mu * X - D * X               # cells, e.g. algae
      dx2 <-  D *(S0 - S) - 1/Y * mu * X  # substrate, e.g. phosphorus
      list(c(dx1, dx2))
    })
  },
  parms = c(
    vm = 1.0,           # max growth rate, 1/d
    km = 2.0,           # half saturation constant, mumol / L
    Y  = 100,           # cells /mumol Substrate
    D  = 0.5,           # dilution rate, 1/d
    S0 = 10             # substrate in inflow, mumol / L
  ),
  times = c(from=0, to=40, by=.5),
  init  = c(X=10, S=10), # cells / L; Substrate umol / L
  solver = "lsoda"
)

}

\keyword{datasets}
