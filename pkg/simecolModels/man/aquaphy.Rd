\name{Aquaphy}
\alias{Aquaphy}

\title{a physiological model of unbalanced algal growth}
\description{Phytoplankton model with uncoupled carbon and nitrogen assimilation.
\cr Algal biomass is described via 3 different state variables:
\item[\code{LMW}] low molecular weight carbohydrates, the product of photosynthesis,
\item[\code{RESERVE}] storage molecules and
\item[\code{PROTEINS}] the biosynthetic and photosynthetic apparatus.

All these state variables are expressed in mmol C m-3.
Only proteins contain nitrogen and chlorophyll, at a fixed amount (i.e. using a fixed stoichiometric ratio).
As the relative amount of proteins changes in the algae, so does the N:C and the Chl:C ratio.
\cr An additional state variable, dissolved inorganic nitrogen (DIN) has units of mmol N m-3.}
\usage{Aquaphy()}
\format{An S4 object according to the \code{\link[simecol]{odeModel}} specification. 
\cr The object contains the following slots:


\item \code{main}  Aquaphy model specifications.
\item \code{parms}  Vector with the named parameters of the model - see code
\item \code{times}  Simulation time and integration interval.
\item \code{init}  Vector with start values for algal state variables and DIN.

}
\author{Karline Soetaert}
\examples{

# initialize model
aquaphy <- Aquaphy()

# show model code, parameter settings,...
print(aquaphy)

# run simulation
out       <- out(sim(aquaphy))

# plot results
par(mfrow=c(2,2), oma=c(0,0,3,0))
col <- grey(0.9)

plot (out$time, out$Chlorophyll, type="l", main="Chlorophyll",
      xlab="time, hours", ylab=expression(paste(mu, g/l)))
polygon(out$time, out$PAR-10, col=col, border=NA)
box()
lines (out$time, out$Chlorophyll, lwd=2 )


plot (out$time, out$DIN, type="l",main="DIN",
      xlab="time, hours", ylab="mmolN/m3")
polygon(out$time,out$PAR-10,col=col,border=NA);box()
lines (out$time,out$DIN  ,lwd=2 )


plot (out$time, out$NCratio, type="n", main="NCratio",
      xlab="time, hours", ylab="molN/molC")
polygon(out$time, out$PAR-10, col=col, border=NA)
box()
lines (out$time, out$NCratio, lwd=2 )


plot (out$time, out$PhotoSynthesis, type="l", main="PhotoSynthesis",
      xlab="time, hours", ylab="mmolC/m3/hr")
polygon(out$time, out$PAR-10, col=col, border=NA);box()
lines (out$time, out$PhotoSynthesis, lwd=2 )

mtext(outer=TRUE, side=3, "AQUAPHY", cex=1.5)
# SUMMARY  model output: #

t(summary(out))

}
\references{Chapter 2.8.2 from the book by Soetaert and Herman, 2008. 
  A practical guide to ecological Modelling. Springer.

Lancelot, C., Veth, C. and Mathot, S. (1991). Modelling ice-edge phytoplankton 
  bloom in the Scotia-Weddel sea sector of the Southern Ocean during spring 1988. 
  Journal of Marine Systems 2, 333-346.
}
\seealso{R-package simecol for a description of the simObj class}
\keyword{ misc }

