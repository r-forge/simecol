\name{NPZD}
\alias{NPZD}

\title{an NPZD model}
\description{model describing nutrients (N), phytoplankton (P), zooplankton (Z),
and detritus (D).}

\usage{NPZD()}
\format{An S4 object according to the \code{\link[simecol]{odeModel}} specification. 
\cr The object contains the following slots:

\describe{
\item[\code{main}] NPZD model specifications.
\item[\code{parms}] Vector with the named parameters of the model - see code
\item[\code{times}] Simulation time and integration interval.
\item[\code{init}] Vector with start values for state variables
}}
\author{Karline Soetaert}
\examples{

# initialize model
myNPZD <- NPZD()

# show model code, parameter settings,...
print(myNPZD)

#----------------------#
# RUNNING the model:   #
#----------------------#
#  2 steps
#  first year = spinup

res   <- out(sim(myNPZD))
num          <- nrow(res)   # last element
#  step 2  : the model is reinitialised with the final conditions of previous run
init(myNPZD) <- c(PHYTO   =res$PHYTO[num],
                  ZOO     =res$ZOO[num],
                  DETRITUS=res$DETRITUS[num],
                  DIN     =res$DIN[num])

# step 2: another 2 years simulated
Times <-seq(0,730,by=1)
times(myNPZD)  <- Times
res   <- out(sim(myNPZD))

#------------------------#
# PLOTTING model output: #
#------------------------#
windows()
par(mfrow=c(2,2), oma=c(0,0,3,0))   # set number of plots (mfrow) and margin size (oma)

plot (Times,res$PAR        ,type="l",lwd=2,main="PAR"        ,xlab="time, hours",ylab="microEinst/m2/s")
plot (Times,res$Chlorophyll,type="l",lwd=2,main="Chlorophyll",xlab="time, hours",ylab="microg/l")
plot (Times,res$ZOO        ,type="l",lwd=2,main="Zooplankton",xlab="time, hours",ylab="mmolN/m3")
plot (Times,res$DIN        ,type="l",lwd=2,main="DIN"        ,xlab="time, hours",ylab="mmolN/m3")

mtext(outer=TRUE,side=3,"NPZD model",cex=1.5)

}
\references{Chapter 2.8.1. from the book by Soetaert and Herman, 2008.
  A practical guide to ecological Modelling. Springer.

}
\seealso{R-package simecol for a description of the simObj class}
\keyword{ misc }

