\name{ZooScheldt}
\alias{ZooScheldt}

\title{Fate of marine zooplankton in the Scheldt estuary}
\description{Model describing the dynamics of marine zooplankton in the scheldt estuary.

The original model was published in Soetaert and herman (1994). The implementation
in R is described in Soetaert and Herman (2008), chapter 6.6.5.

The model equations are:
\deqn{
\frac{{\partial   {\rm{C}}}}{{\partial   t}} =
- \frac{\partial }{{A \cdot \partial   x}}(Q \cdot {\rm{C}})
+ \frac{\partial }{{A \cdot \partial   x}}(A \cdot E_x  \cdot \frac{{\partial  & {\rm{C}}}}{{\partial   x}})
+ g \cdot {\rm{C}}
}{dC/dt=-1/A*(d/dx(Q.C))+1/A*d/dx(A*E_x*(dC/dx))+g*C}

where C is marine zooplankton concentration (gram dry weight/m3), A is estuarine cross-sectional surface area (m2),
Q is freshwater flow (m3/d), Ex is tidal dispersion (m2/d) and x is position along the estuarine length axis.

Marine zooplankton in the Scheldt is driven to the sea by the freshwater flow (1st term) and 
mixed in the estuary through the action of the tides (2nd term). 
The biological effects (3rd term) is respresented by one parameter, the net growht rate (g) of the zooplankton.
If g is positive, there is growth, if negative, there is net decay.

The estuary is subdivided in 100 boxes, extending from the river (box 1) to the sea (box 100).

For the numerical approximation of the partial differential equations, see Soetaert and Herman, 2008
}
\usage{ZooScheldt()}
\format{An S4 object according to the \code{\link[simecol]{odeModel}} specification. 

The object contains the following slots:

\item \code{main}   Model specifications.
\item \code{parms}  Vector with the named parameters of the model - see code
\item \code{times}  Simulation time and integration interval.
\item \code{init}  Vector with start values for the zooplankton state variables.


The model is solved using integration routine \code{\link[deSolve]{ode.1D}} from package \code{\link[deSolve]{deSolve}}.
}
\author{Karline Soetaert}
\examples{
  
#----------------------#
# Create Instance      #
#----------------------#

# Note: lower case letter of the instance and capital letter of the constructor
zooScheldt <- ZooScheldt()  

# show model code, parameter settings,...
print(zooScheldt)

#----------------------#
# RUNNING the model:   #
#----------------------#

# run simulation; parameter "nspec" that is passed to ode.band
outp <- out(sim(zooScheldt, nspec=1))

#----------------------#
# User-defined solver: #
#----------------------#

# It is possible to incorporate simulation parameters
# within a user defined solver function
mysolver <- function(y, times, func, parms) {
    ode.band(y, times=times, func, parms, nspec=1)
}

# run simulation
solver(zooScheldt) <- "mysolver"
outp       <- out(sim(zooScheldt))

#------------------------#
# PLOTTING model output: #
#------------------------#

# Plot zooplankton; first get the data
Dist  <- inputs(zooScheldt)$boxes$Dist
times <- outp$time

par(oma=c(0,0,3,0))   # set margin size (oma)
filled.contour(x=times, y=Dist/1000, z=as.matrix(outp[,-1]),
               color= terrain.colors, xlab="time, days",
               ylab= "Distance, km", main="Zooplankton, mg/m3")
mtext(outer=TRUE, side=3, "Marine Zooplankton in the Scheldt", cex=1.5) 

}
\references{
Soetaert, K and P.M.J. Herman, 2008. A practical guide to ecological Modelling. Using R as a simulation platform. Springer.
(Chapter 6.6.5)

Soetaert and Herman, 1994. One foot in the grave: zooplankton drift into the Westerschelde estuary (The Netherlands).  
Marine Ecology Progress Series 105: 19-29.
}
   
\seealso{R-package \code{\link[simecol]{simecol}} for a description of the 
  \code{\link[simecol]{simObj}} class
}
  
\keyword{ misc }
