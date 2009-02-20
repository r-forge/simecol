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
  main = function(time, init, parms, inputs=NULL) {
    vm  <- parms["vm"] # max growth rate 
    km  <- parms["km"] # half saturation constant
    D   <- parms["D"]  # dilution rate
    S0  <- parms["S0"] # substrate in inflow
    Y   <- parms["Y"]  # yield coefficient for substrate
    X   <- init[1]     # cells (e.g. algae)
    S   <- init[2]     # substrate (e.g. phosphorus)

    mu  <- vm * S/(km + S)              # Monod equation
    dx1 <- mu * X - D * X               # cells 
    dx2 <-  D *(S0 - S) - 1/Y * mu * X  # substrate
    list(c(dx1, dx2))
  },
  parms = c(
    vm = 1.0,           # 1/d
    km = 2.0,           # mumol Substrate/l
    Y  = 100,           # cells /mumol Substrate
    D  = 0.5,           # 1/d
    S0 = 10             #  mumol Substrate/l
  ),
  times = c(from=0, to=40, by=.5),
  init  = c(X=10, S=10), # cells/l; umol Substrate/l                     
  solver = "lsoda"
)

}

\keyword{datasets}
