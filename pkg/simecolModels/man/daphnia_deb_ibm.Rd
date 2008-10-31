\name{daphnia_deb_ibm}
\alias{daphnia_deb_ibm}
\alias{daphnia_deb_phyto}
\alias{daphnia_deb_lifeequations}
%\docType{data}
\title{Bioenergetic Individual-Based Model of Daphnia}
\description{
  The DEB-IBM model is an individual based simulation of a bioenergetic
  Daphnia model and a logistic phytoplankton growth model.
}
\usage{
daphnia_deb_phyto()
daphnia_deb_ibm(submodel = daphnia_deb_phyto())
}

\arguments{
  \item{submodel}{the subobject defining a single time-step of the
    individual-based simulation (i.e. \code{daphnia_deb_phyto} or a
    compatible variant of it)}
}


\value{S4 object according to the \code{\link[simecol]{simObj}} specification.
The object contains the following slots:
    \item{main}{The main equations of the model}
  \item{parms}{a list with the parameters of the model}
  \item{times}{Simulation time and time steps.}
  \item{inputs}{Environmental conditions}
  \item{solver}{Character string with the integration method.}
  \item{init, initfunc}{\code{init} is initialized automatically according to the
     parameter values.}
}

\details{
  To see all details, please have a look into the implementation and the
  original publications.
}

\references{

Rinke, K. (2006).  Species-oriented model approaches to Daphnia spp.: linking 
the individual level to the population level. PhD Thesis. 
Technische Universität Dresden, Fakultät Forst-, Geo- und Hydrowissenschaften.
\url{http://nbn-resolving.de/urn:nbn:de:swb:14-1146736164110-76933}

Rinke, K. & Vijverberg, J. (2005). A Model Approach to Evaluate the Effect of 
Temperature and Food Concentration on Individual Life-history and 
Population Dynamics of Daphnia. Ecological Modelling, 186, 326-344

Rinke, K.; Vijverberg, J.; Petzoldt, T. & Benndorf, J. (2005). Individual 
and population level dynamics of Daphnia at varying conditions of food, 
temperature and fish predation: a model approach.
Verh. Internat. Verein. Limnol.,  29, 310-314

Petzoldt, T. and K. Rinke (2007).  simecol: An
Object-Oriented Framework for Ecological
Modeling in R.  Journal of Statistical
Software, 22(9), 1--31.  URL
\url{http://www.jstatsoft.org/v22/i09/}.

}


\seealso{
  \code{\link[simecol]{sim}},
  \code{\link[simecol]{parms}},
  \code{\link[simecol:parms]{init}},
  \code{\link[simecol:parms]{times}}.
}

\examples{

## load individual-level model (DEB and phyto, differential equations)
submodel <- daphnia_deb_phyto()

## test one outer step alone
m <- sim(submodel)
out(m)

### create individual-based model (IBM, discrete, population dynamics)
### with a newly initialized daphnia_deb_phyto() object as submodel
deb_ibm <- daphnia_deb_ibm()

## set simulation parameters
times(deb_ibm)["to"] <- 2
## the following takes a while !
\dontrun{
times(deb_ibm)["to"] <- 200
}
deb_ibm <- sim(deb_ibm)
o <- out(deb_ibm)


### plot results
par(mfrow=c(3,1))
plot(o$time, o$x1, type="l",
  main="Phytoplankton", col="red", ylim=c(0,0.5),xlab="Day",ylab="mg C / L")
lines(o$time, o$x2, col="blue")
lines(o$time, o$x3, col="green")
plot(o$time, o$abundance, type="l", main="Abundance",xlab="Day", ylab="Ind / L")
bm.mgc <- o$weight * o$abund / 1000
plot(o$time, bm.mgc , type="l", main="Biomass",xlab="Day", ylab="mg C / L")
}
\keyword{misc}
