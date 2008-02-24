\name{chemostat}
\docType{data}
\alias{chemostat}
\title{chemostat Model}
\description{
  \pkg{simecol} example: Model of continuos culture of microorganisms (chemostat).
}
\usage{data(chemostat)}

\format{An S4 object according to the \code{\link{odeModel}} specification. 
The object contains the following slots: 

\describe{
  \item{\code{main}}{The differential equations for substrate (\code{S}) and cells (\code{X}).}
  \item{\code{parms}}{A vector with the named parameters of the model:
       \describe{
          \item{\code{vm}}{Maximum growth rate of the cells.}
          \item{\code{km}}{Half saturation constant.}
          \item{\code{Y}}{Yield coefficient (conversion factor of substrate into cells).}
          \item{\code{D}}{Dilution rate.}
          \item{\code{S0}}{Substrate concentration in the inflow.}          
       }}
  \item{\code{times}}{Simulation time and integration interval.}
  \item{\code{init}}{Vector with start values for substrate (\code{S}) and cells (\code{X}).}
} 
To see all details, please have a look into the implementation.
}


\seealso{
  \code{\link{simecol-package}},
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}



\examples{
  data(chemostat)
  plot(sim(chemostat))

  parms(chemostat)["D"] <- 0.9 
  plot(sim(chemostat))
}

\keyword{datasets}
