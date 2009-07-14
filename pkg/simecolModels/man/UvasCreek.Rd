\name{UvasCreek}
\alias{UvasCreek}

\title{
  Strontium injection experiment in Uvas Creek, a small stream (cross section
  0.5 m2, length 619 m) in North Carolina.
}
\description{
  Model describing a Strontium injection experiment in Uvas Creek, a small
  stream (cross section 0.5 m2, length 619 m) in North Carolina.

  (Bencala and Walters 1983).
  
  Originally implemented in the OTIS modelling framework (Runkel, 1989)
  (application 2)
  
  Strontium was injected at a constant rate in the upstream part of this
  creek for 3 hours, to reach a maximal concentration of 1.7 mg L-1,
  whereas the background concentration was 0.13 mg L-1.

  The passing of the concentration pulses was assessed at various river
  positions downstream (38, 105, 281, 433 and 619 m).

  The model describes Strontium in a free-flowing zone and a stagnant zone
  and in the streambed, where strontium is absorbed.
  
  The stream is subdivided into five segments with different cross-sectional
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
    \item \code{inputs} External forcings, the tracer concentration that
     is injected at the upstream boundary
  }

  The model is solved using integration routine \code{\link[deSolve]{ode.1D}}
  from package \code{\link[deSolve]{deSolve}}.
}

\author{
  Karline Soetaert
}

\examples{
  
#----------------------#
# Create Instance      #
#----------------------#

# Note: lower case letter of the instance and capital letter of the constructor
uvasCreek <- UvasCreek()

# show model code, parameter settings,...
print(uvasCreek)

#----------------------#
# RUNNING the model:   #
#----------------------#

# run simulation
outp <- out(sim(uvasCreek, nspec=3))

#------------------------#
# PLOTTING model output: #
#------------------------#
Data38 <- matrix (nc=2, byrow=TRUE, data = c(8.,0.143,9.,1.528,
10.,1.59,11.,1.599,12.,0.229,13.,0.173,14.,0.173,15.,0.140,
16.17,0.159,16.68,0.156,17.68,0.165,18.68,0.151,
19.68,0.155,20.68,0.158,21.68,0.134))

Data281 <- matrix (nc=2, byrow=TRUE, data = c(7.8,0.131,9.,0.134,
10.,0.135,11.,0.696,12.,0.946,13.,1.021,14.,0.486,15.,0.288,
16.,0.205,17.,0.207,18.37,0.185,19.37,0.178,20.37,0.174,21.37,
0.168,22.37,0.165,23.37,0.159))

Data433 <- matrix (nc=2, byrow=TRUE, data = c(9.,0.133,10,0.152,
11,0.131,12,0.359,13,0.557,14,0.684,15,0.524,16,0.349,
17,0.240,18,0.218,19.5,0.175,20.5,0.173,21.5,0.168,22.5,0.154))


# Plot strontium; first get the data
Dist  <- inputs(uvasCreek)$boxes$x
times <- outp$times/3600

par(mfrow=c(2,2))
par(oma=c(0,0,3,0))   # set margin size (oma)
image(x=times, y=Dist, z=outp$C,
      col= terrain.colors(100), xlab="time, hour",
      ylab= "Distance, m", main="Strontium")
mtext(outer=TRUE, side=3, "Uvas Creek", cex=1.5)

plot(times,outp$C[,38],  xlab="time, hour", ylab="mg/L",
  main="38m", type="l")
points(Data38, pch = 18)

plot(times,outp$C[,281], xlab="time, hour", ylab="mg/L",
  main="281m", type="l")
points(Data281, pch = 18)

plot(times,outp$C[,433], xlab="time, hour", ylab="mg/L",
  main="433m", type="l")
points(Data433, pch = 18)
}
\references{

Bencala, K.E., and Walters, R.A., 1983, Simulation of solute transport
in a mountain pool-and-riffle stream-A transient storage model:
Water Resources Research, v. 19, no. 3, p. 718-724.

Runkel, R.L., 1998. One-dimensional transport with inflow and storage (OTIS):
a solute transport model for streams and rivers. US geological Survey.
Water-Resources Investigations Report 98-4018. Denver, Colorado.
}
   
\seealso{
  R-package \code{\link[simecol]{simecol}} for a description of the
  \code{\link[simecol]{simObj}} class
}
  
\keyword{ misc }
