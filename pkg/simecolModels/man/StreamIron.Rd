\name{StreamIron}
\alias{StreamIron}

\title{
  Strontium injection experiment in Uvas Creek, a small stream (cross section
  0.5 m2, length 619 m) in North Carolina.
}
\description{
  Model describing Iron (Fe) in St. Kevin Gulch, a 1904 m long headwater
  stream in the Rocky Mountains.
  In a certain stretch, this stream receives metal-rich waters from a large
  mine dump, and at a constant rate.

  Originally implemented in the OTIS modelling framework (Runkel, 1989)
  (application 6)
  
  The model describes Fe in a free-flowing zone and in a stagnant zone.
  
  The stream is subdivided into seven segments with different cross-sectional
  areas, storage zone areas and exchange parameters.

}
\usage{UvasCreek()}

\format{
  An S4 object according to the \code{\link[simecol]{odeModel}} specification.

  The object contains the following slots:
  \itemize{
    \item \code{main}  Model specifications.
    \item \code{parms} Vector with the named parameters of the model -
      see code
    \item \code{times} Simulation time and integration interval.
    \item \code{init}  Vector with start values for the
      state variables.
  }

  The model is solved using steady-state solver \code{\link[rootSolve]{steady.1D}}
  from package \code{\link[rootSolve]{rootSolve}}.
}

\author{
  Karline Soetaert
}

\examples{
  
#----------------------#
# Create Instance      #
#----------------------#

# Note: lower case letter of the instance and capital letter of the constructor
streamIron <- StreamIron()

# show model code, parameter settings,...
print(streamIron)

#----------------------#
# RUNNING the model:   #
#----------------------#

# run simulation
outp <- out(sim(streamIron))

#------------------------#
# PLOTTING model output: #
#------------------------#

Data <- data.frame(Dist = c(
 26  ,363 ,413 ,421 ,439 ,455 ,464 ,484 ,526 ,586 ,641 ,701 ,754 ,781 ,881 ,
 948 ,1068,1265,1308,1421,1497,1557,1677,1767,1776,1804  ),
 Fe = c(
 0.64, 0.63, 1.5 , 2.5 , 5.6 , 12  , 12  , 13  , 5.0 , 4.4 , 4.2 , 3.9 , 3.8 ,
 3.4 , 2.9 , 1.4 , 2.5 , 1.7 , 1.8 , 1.7 , 1.6 , 1.3 , 1.3 , 1.2 , 1.4 , 1.4 )
)

# Plot Fe concentration
Dist  <- inputs(streamIron)$boxes$x
plot(Dist,outp[,"Fe"],xlab="Distance, m", ylab="Fe conc",type="l",
  ylim=c(0,13),main="St. Kevin Gulch")
points(Data,pch=15,cex=1.5)
}
\note{
The original parameter settings from the OTIS application are used,
except for lambda, which has been fitted to the data.
}
\references{

Runkel, R.L., 1998. One-dimensional transport with inflow and storage (OTIS):
a solute transport model for streams and rivers. US geological Survey.
Water-Resources Investigations Report 98-4018. Denver, Colorado.
}
   
\seealso{
  R-package \code{\link[simecol]{simecol}} for a description of the
  \code{\link[simecol]{simObj}} class
}
  
\keyword{ misc }
