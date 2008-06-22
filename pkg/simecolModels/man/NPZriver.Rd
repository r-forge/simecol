\name{NPZriver}
\alias{NPZriver}

\title{Nutrient Phytoplankton Zooplankton (NPZ) model in a river}
\description{Model describing the dynamics of Nutrients Phytoplankton and 
 Zooplankton in a river. (Soetaert and Herman, 2008).

The model describes 
\describe{
  \item Nutrients,  
  \item Phytoplankton and 
  \item Zooplankton (NPZ model). 
}

  The physical component consists of an advective transport term (river flow) only. 

  The river has constant cross-sectional surface and is 100 km long. It is subdivided in 100 boxes.
  
  For the numerical approximation of the partial differential equations, see Soetaert and Herman, 2008
}
\usage{NPZriver()}
\format{An S4 object according to the \code{\link[simecol]{odeModel}} specification. 

The object contains the following slots:

\describe{
\item[\code{main}]  Model specifications.
\item[\code{parms}] Vector with the named parameters of the model - see code
\item[\code{times}] Simulation time and integration interval (if dynamic simulation); 
  not used if the steady-state is estimated.
\item[\code{solver}]  User supplied solver function that calls \code{steady.1D}
  with appropriate simulation control parameters and re-arranges output data.
\item[\code{initfunc}] Function that initialises the state variables and calculates 
  the box lenghts.
}
The model is solved to steady-state using steady-state solver 
\code{\link[rootSolve]{steady.1D}} from package \code{\link[rootSolve]{rootSolve}}.
}
\author{Karline Soetaert}
\examples{

# create an instance of the model
myNPZriver <- NPZriver()

# show model code, parameter settings,...
print(myNPZriver)

# Note that the model has a specialized solver function built in:
solver(myNPZriver)

# Alternative way: 
#   use standard solver and pass additional parameters through sim
# solver(myNPZriver) <- "NPZ_steady.1D"
# myNPZriver <- sim(myNPZriver, nspec=3, pos=TRUE)


#====================#
# 3 Model runs       #
#====================#

# steady-state solution, v=1 km d-1
outp       <- out(sim(myNPZriver))
N <- outp$N
P <- outp$P
Z <- outp$Z

# steady-state solution, v=5 km d-1
parms(myNPZriver)["flow"]     <- 5        # river flow, km/day
outp       <- out(sim(myNPZriver))
N <- cbind(N, outp$N)
P <- cbind(P, outp$P)
Z <- cbind(Z, outp$Z)


# steady-state solution, v=10 km d-1
parms(myNPZriver)["flow"]     <- 10     
outp       <- out(sim(myNPZriver))
N <- cbind(N, outp$N)
P <- cbind(P, outp$P)
Z <- cbind(Z, outp$Z)

#====================#
# plotting           #
#====================#
par(mfrow=c(2, 2))
parms <- as.list(parms(myNPZriver)) 
delx  <- parms$riverlen / parms$Nb        # box length
Dist  <- seq(delx/2, parms$riverlen, by=delx)
          
matplot(Dist, N, ylab="mmolN/m3", main="DIN", type="l",
        lwd=2, ylim=c(0,110), xlab="Distance, km")
matplot(Dist, P, ylab="mmolN/m3", main="Phytoplankton", type="l",
        lwd=2, ylim=c(0,110), xlab="Distance, km")
matplot(Dist, Z, ylab="mmolN/m3", main="Zooplankton", type="l",
        lwd=2, ylim=c(0,110), xlab="Distance, km")

plot(0, type="n", xlab="", ylab="", axes=FALSE)
legend("center", lty=c(1, 1, 2), lwd=c(2, 1, 1), col=1:3,
        title="river flowrate",
       c(expression(v==1~km~d^{-1}), expression(v==5~km~d^{-1}),
         expression(v==10~km~d^{-1}))
) 

mtext(side=3, outer=TRUE, "NPZ model in a river", line=-1.5, cex=1.5)	

}
\references{
Chapter 7.9.5  from the book by Soetaert and Herman, 2008. A practical guide to ecological Modelling. Springer.
}

\seealso{R-package \code{\link[simecol]{simecol}} for a description of the 
  \code{\link[simecol]{simObj}} class
}

\keyword{ misc }
