\name{dvm_phyto}
\alias{dvm_phyto}

\title{Phytoplankton Growth Model with Pulsed Grazing}
\description{
  The model simulates a system with two functional groups of phytoplankton
  and one vertically migrating zooplankton population.
}
\usage{
dvm_phyto()
}
\value{S4 object according to the \code{\link[simecol]{odeModel}} specification.
The object contains the following slots:
  \item{main}{The differential equations for two phytoplankton groups,
    zooplankton and phosporus:
    \describe{
      \item{Xr}{biovolume of r-strategist (mg/L),}
      \item{Xk}{biovolume of k-strategist (mg/L),}
      \item{Z}{biovolume of zooplankton (mg/L),}
      \item{P}{phosphorus concentration (mug/L),}
    }
  }
  \item{parms}{Vector with the named parameters of the model, see
   tables in original publication}
  \item{times}{Simulation time and integration interval.}
  \item{init}{Vector with start values for the state variables.}
  \item{solver}{Character string with the integration method.}
}

\references{
Rudolf, L., Petzoldt, T., Rinke, K. and
Benndorf, J. (submitted). Effects of zooplankton diel vertical migration
on a phytoplankton community: a scenario analysis of the underlying mechanisms.
}
\examples{

# Scenario analysis for the P - X1/X2 - Z - Lake Model with
#  diurnal vertical migration of Daphnia (Z)
# (Rudolf et. al. submitted)

##   --> uncomment the following line  for Series 2,
##       with completely inedible algae (type "X3")
# parms(dvmphyto)["ZXk.graz.max"] <- 0

sc1 <- sc2 <- sc3 <- sc4 <- sc5 <- dvmphyto <- dvm_phyto()

## show parameters, note that several depend on others
parms(sc1)
initfunc(sc1)

parms(sc1)["DVM"] <- FALSE
parms(sc2)["DVM"] <- TRUE

parms(sc3)["DVM"] <- TRUE
parms(sc3)["ZXr.graz.max.ind"] <- parms(dvmphyto)["ZXr.graz.max.ind"] * 3
parms(sc3)["ZXk.graz.max.ind"] <- parms(dvmphyto)["ZXk.graz.max.ind"] * 3

parms(sc4)["DVM"] <- TRUE
parms(sc4)["ZXr.graz.max.ind"] <- parms(dvmphyto)["ZXr.graz.max.ind"] * 3
parms(sc4)["ZXk.graz.max.ind"] <- parms(dvmphyto)["ZXk.graz.max.ind"] * 3
parms(sc4)["Z.mort.min.d"]     <- parms(dvmphyto)["Z.mort.min.d"]     * 3
parms(sc4)["Z.mort.temp.d"]    <- parms(dvmphyto)["Z.mort.temp.d"]    * 3
parms(sc4)["f.Z.resp"]         <- 3

parms(sc5)["DVM"] <- FALSE
parms(sc5)["ZXr.graz.max.ind"] <- parms(dvmphyto)["ZXr.graz.max.ind"] / 3
parms(sc5)["ZXk.graz.max.ind"] <- parms(dvmphyto)["ZXk.graz.max.ind"] / 3
parms(sc5)["Z.mort.min.d"]     <- parms(dvmphyto)["Z.mort.min.d"]     / 3
parms(sc5)["Z.mort.temp.d"]    <- parms(dvmphyto)["Z.mort.temp.d"]    / 3
parms(sc5)["f.Z.resp"]         <- 3


## it is also possible to use "chained cloning" e.g. to clone
## sc5 as copy of sc4 and then to modify the DVM parameter only

## now we simulate the scenarios one after one

\dontrun{
## the simulations take a while, so we skip during in the package check
o1 <- out(sim(sc1))
o2 <- out(sim(sc2))
o3 <- out(sim(sc3))
o4 <- out(sim(sc4))
o5 <- out(sim(sc5))

## you can now create arbitrary plots and analyses using standard R functions

plot(o1$time/24, o1$Xr, type="l", ylim=c(0,1))
lines(o1$time/24, o1$Xk, col="red")
}


}
\keyword{ misc }
