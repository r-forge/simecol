\name{mcyst_chemostat}
\alias{mcyst_chemostat}

\title{Microcystin Production Model (Chemostat Version)}
\description{
  This is a chemostat version of the MCYST model with substrate (phosphorus)
  dependend growth.
}
\usage{
mcyst_chemostat()
}
\value{S4 object according to the \code{\link[simecol]{odeModel}} specification.
The object contains the following slots:
    \item{main}{The differential equations for cell abundance and
      Microcystin concentration:
      \describe{
      % check measurement units !!! !!! !!! !!! !!!
        \item{cells}{abundance of cells (1/L),}
        \item{phos}{phosphorus concentration (nmol/L).}
        \item{mcyst}{microcystin concentration (fg/L).}
      }
   }
  \item{parms}{Vector with the named parameters of the model:
    \describe{
      \item{mu.m}{max. growth rate (1/d),}
      \item{km}{half sat. constant (mumol/L),}
      \item{Y}{Yield coefficient,}
      \item{p}{MCYST production coefficient (fg/cell),}
      \item{dM}{MCYST decay rate (1/d),}
      \item{S0}{phosphorus in import (umol/L),}
      \item{D}{dilution rate of chemostat (1/d).}
    }
  }
  \item{times}{Simulation time and integration interval.}
  \item{init}{Vector with start values for \code{cells}, \code{phos} and \code{mcyst}.}
  \item{solver}{Character string with the integration method.}
}

\details{
  To see all details, please have a look into the implementation and the
  original publications.
}
\references{
  Jähnichen, S., Petzoldt, T. and Benndorf, J. (2001). Evidence for Control of
  Microcystin Dynamics in Bautzen Reservoir (Germany) by Cyanobacterial
  Population Growth Rates and Dissolved Inorganic Carbon.
  \emph{Archiv für Hydrobiologie}, \bold{2}, 150, 177-196.
  
  Jähnichen, S., Ihle, T. and Petzoldt, T. (2008).
  Variability of microcystin cell quota: A small model explains dynamics and
  equilibria. \emph{Limnologica - Ecology and Management of Inland Waters},
  \bold{38}, 339-349. URL \url{http://dx.doi.org/10.1016/j.limno.2008.05.003}
}
\seealso{
  \code{\link[simecol]{sim}},
  \code{\link[simecol]{parms}},
  \code{\link[simecol:parms]{init}},
  \code{\link[simecol:parms]{times}}.
}
\examples{
## create model object
m <- mcyst_chemostat()
## inspect parameters, start values, time steps
parms(m)
init(m)
times(m)
## simulate the model
m <- sim(m)
o <- out(m)

par(mfrow=c(2,2))
plot(o$time, o$cells, col="green", type="l",
  xlab="time", ylab="algae", ylim=c(0, max(o$cells)))
plot(o$time, o$phos,  col="blue",  type="l",
  xlab="time", ylab="substrate")
plot(o$time, o$mcyst/1000, col="cyan",  type="l",
  xlab="time", ylab="mcyst mug/l")
plot(o$time, o$mcyst/o$cells, col="red",  type="l",
  xlab="time", ylab="spec. mcyst")
}

\keyword{ misc }