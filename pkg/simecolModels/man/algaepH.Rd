\name{algaepH}
\alias{algaepH}

\title{an algal model with pH dynamics}
\description{Phytoplankton model which includes pH dynamics.

The algae are either growing on nitrate or on ammonium.}


\usage{algaepH()}
\format{An S4 object according to the \code{\link[simecol]{odeModel}} specification. 

The object contains the following slots:

\item \code{main}  algaepH model specifications.
\item \code{parms}  Vector with the named parameters of the model - see code
\item \code{times}  Simulation time and integration interval.
\item \code{init}  Vector with start values for state variables
}
\author{Karline Soetaert}
\examples{

# initialize model
myAlgaepH <- algaepH()

# show model code, parameter settings,...
print(myAlgaepH)

# run simulation
out    <- out(sim(myAlgaepH))                  # algae growing on nitrate
out2   <- out(sim(myAlgaepH,Nitrate=FALSE))    # algae growing on ammonium

# plot results

#------------------------#
# PLOTTING model output: #
#------------------------#

par(mfrow=c(2,2), oma=c(0,0,3,0))         # set number of plots (mfrow) and margin size (oma)
plot (out$time,out$ALGAE,type="l",main="Algae",xlab="time, hours",ylab="mmol/m3")
polygon(out$time,out$PAR-10,col="lightgrey",border=NA)
box()
lines (out$time,out$ALGAE  ,lwd=2 )

plot (out$time,out$DIN ,type="l",main="DIN"  ,xlab="time, hours",ylab="mmolN/m3", lwd=2)
plot (out$time,out$DIC ,type="l",main="DIC"  ,xlab="time, hours",ylab="mmolC/m3", lwd=2)
plot (out$time,out$pH,type="l",main="pH"  ,xlab="time, hours",ylab="-", lwd=2)
lines(out2$time,out2$pH,col="red")
legend("bottomright",col=c("black","red"), lwd=c(2,1),c("nitrate","ammonium"))
mtext(outer=TRUE,side=3,"Algal growth and pH",cex=1.5)

}
\references{Chapter 8.5.2. from the book by Soetaert and Herman, 2008.
  A practical guide to ecological Modelling. Springer.

}
\seealso{R-package simecol for a description of the simObj class}
\keyword{ misc }

