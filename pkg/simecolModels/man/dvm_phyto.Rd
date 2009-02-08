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
Petzoldt, T., Rudolf, L., Rinke, K. and
Benndorf, J. (accepted). Effects of zooplankton diel vertical migration
on a phytoplankton community: a scenario analysis of the underlying
mechanisms. Ecological Modelling.
}
\examples{

# Scenario analysis for the P - X1/X2 - Z - Lake Model with
#  diurnal vertical migration of Daphnia (Z)

sc1 <- sc2 <- sc3 <- sc4 <- sc5 <- dvmphyto <- dvm_phyto()

##   --> uncomment the following line  for Series 2,
##       with completely inedible algae (type "X3")
# parms(dvmphyto)[["ing_max_i"]][2] <- 0

## show parameters
# parms(sc1)

## define scenarios
parms(sc1)["DVM"] <- FALSE

parms(sc2)["DVM"] <- TRUE

parms(sc3)["DVM"] <- TRUE
parms(sc3)[["ing_max_i"]] <- parms(dvmphyto)[["ing_max_i"]]  * 3

parms(sc4)["DVM"] <- TRUE
parms(sc4)[["ing_max_i"]] <- parms(dvmphyto)[["ing_max_i"]]  * 3
parms(sc4)[["resz_max"]]  <- parms(dvmphyto)[["resz_max"]]   * 3
parms(sc4)[["mort_max"]]  <- parms(dvmphyto)[["mort_max"]]   * 3


parms(sc5)["DVM"] <- FALSE
parms(sc5)[["ing_max_i"]] <- parms(dvmphyto)[["ing_max_i"]]  / 3
parms(sc5)[["resz_max"]]  <- parms(dvmphyto)[["resz_max"]]   / 3
parms(sc5)[["mort_max"]]  <- parms(dvmphyto)[["mort_max"]]   / 3


## it is also possible to use "chained cloning" e.g. to clone
## sc5 as copy of sc4 and then to modify the DVM parameter only

## now we simulate the scenarios one after one

o1 <- out(sim(sc1))

\dontrun{
## the simulations take a while, so we skip some in package check
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
