\encoding{latin1}
\name{mcyst_batch}
\alias{mcyst_batch}

\title{Microcystin Production Model (Batch Version)}
\description{
  This is a batch version of the MCYST model with logistic growth.
}
\usage{
mcyst_batch()
}
\value{S4 object according to the \code{\link[simecol]{odeModel}} specification.
The object contains the following slots:
  %\describe{
    \item{main}{The differential equations for cell abundance and
      Microcystin concentration:
      \describe{
        \item{cells}{abundance of cells (1/L),}
        \item{mcyst}{microcystin concentration (fg/L).}
      }
    }
  %}
%  \item{equations}}{all equations are implemented in main}
  \item{parms}{Vector with the named parameters of the model:
    \describe{
      \item{mu}{Growth rate of cells (1/d),}
      \item{K}{Carrying Capacity (cells /L),}
      \item{p}{MCYST production coefficient (fg/cell),}
      \item{dM}{MCYST depletion rate (1/d),}
      \item{s}{sedimentation rate (1/d).}
    }
  }
  \item{times}{Simulation time and integration interval.}
  \item{init}{Vector with start values for \code{cells} and \code{mcyst}.}
  \item{solver}{Character string with the integration method.}
}

\details{
  To see all details, please have a look into the implementation and the
  original publications.
}
\references{
  J\enc{ä}{ae}hnichen, S., Petzoldt, T. and Benndorf, J. (2001). Evidence for Control of
  Microcystin Dynamics in Bautzen Reservoir (Germany) by Cyanobacterial
  Population Growth Rates and Dissolved Inorganic Carbon.
  \emph{Archiv f\enc{ü}{ue}r Hydrobiologie}, \bold{2}, 150, 177-196.
  
  J\enc{ä}{ae}hnichen, S., Ihle, T. and Petzoldt, T. (2008).
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
m <- mcyst_batch()
## inspect parameters, start values, time steps
parms(m)
init(m)
times(m)
## simulate the model
m <- sim(m)
o <- out(m)

par(mfrow=c(3,1))
plot(o$time, o$cells, col="green", type="l",
  xlab="", ylab="Microcystis", ylim=c(0, max(o$cells)))
plot(o$time, o$mcyst/1000, col="cyan",  type="l",
  xlab="time (d)", ylab="Microcystin mug/l")
plot(o$time, o$mcyst/o$cells, col="red",  type="l",
  xlab="time", ylab="spec. MCYST")
}

\keyword{ misc }