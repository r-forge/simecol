\name{SiDia}
\alias{SiDia}

\title{Silicate diagenesis in marine sediments}
\description{Model describing the dynamics of biogenic and dissolved silicate
 in a marine sediment. (Soetaert and Herman, 2008).

The orignal model was described in (Schink et al., 1975). It
was one of the first models dealing with the early diagenesis of silica.

The model is described in Soetaert and Herman (2008), chapter 3.6.5.
Its R-implementation is in chapter 7.8.5.

Biogenic silicate (BSi), expressed in micromol l-1 solid, is mixed in the sediment (Bioturbation, 1st term)
and dissolves (2nd term).
The dissolution is first-order with BSi concentration and
decreases linearly with increasing dissolved silicate concentration,
until an equilibrium concentration (eqSi) is reached.

At the sediment-water interface, an amount of BSi is deposited
(flux boundary condition). the deep boundary condition is a zero-gradient condition.


Dissolved silicate, in micromol l-1 liquid, and mixed by molecular diffusion (1st term), and produced by dissolution.
At the upper boundary, a bottom water concentration is prescribed. At large depths, a zero-gradient boundary is imposed.


The model equations are:
\deqn{
\frac{{\partial {\rm{BSi}}}}{{\partial t}} = \frac{1}{{1 - \Phi _x }}\frac{\partial }{{\partial x}}[(1 - \Phi _x ) \cdot D_b \frac{{\partial {\rm{BSi}}}}{{\partial x}}] - \lambda  \cdot {\rm{BSi}} \cdot (1 - \frac{{{\rm{DSi}}}}{{eqSi}})
}{dBSi/dt = 1/(1-phi_x) * (d/dx[(1-phi_x)*Db*dBSi/dx)]-lambda*BSi*(1-DSi/eqSi)}
\deqn{
Flux_0  =  - (1 - \Phi _0 ) \cdot D_b \left. {\frac{{\partial {\rm{BSi}}}}{{\partial x}}} \right|_0  }{Flux_0=(1-phi_0)*Db*(dBSi/dx)_0}

\deqn{
 \left. {\frac{{\partial {\rm{BSi}}}}{{\partial x}}} \right|_\infty   = 0 }{dBSi/dx|infinity=0}

for biogenic silicate and

\deqn{
\frac{{\partial {\rm{DSi}}}}{{\partial t}} = \frac{1}{{\Phi _x }}\frac{\partial }{{\partial x}}[\Phi _x  \cdot D_s \frac{{\partial {\rm{DSi}}}}{{\partial x}}] +
\lambda  \cdot {\rm{BSi}} \cdot (1 - \frac{{{\rm{DSi}}}}{{eqSi}}) \cdot \frac{{1 - \Phi_x}}{\Phi_x}}{dDSi/dt = 1/phi_x * (d/dx((phi_x)*Ds*dDSi/dx))+lambda*BSi*(1-DSi/eqSi)*(1-phi_x)/phi_x)}

\deqn{
 {\rm{DSi}}_0  = BW }{DSi|0=BW}

\deqn{
 \left. {\frac{{\partial {\rm{DSi}}}}{{\partial x}}} \right|_\infty   = 0 }{dDSi/dx|infinity=0}


for dissolved silicate.

For the numerical approximation of these partial differential equations, see Soetaert and Herman, 2008
}
\usage{SiDia()}
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
  the sediment grid, porosity and bioturbation profiles.
}
The model is solved to steady-state using steady-state solver 
\code{\link[rootSolve]{steady.1D}} from package \code{\link[rootSolve]{rootSolve}}.
}
\author{Karline Soetaert}
\examples{

# create an instance of the model
mySiDia <- SiDia()

# show model code, parameter settings,...
print(mySiDia)

# Note that the model has a specialized solver function built in:
solver(mySiDia)

# Alternative way: 
#   use standard solver and pass additional parameters through sim
# solver(mySiDia) <- "SiDia_steady.1D"
# mySiDia <- sim(mySiDia, nspec=2, pos=TRUE)


#====================#
# 3 Model runs       #
#====================#

# three runs with different deposition rates
parms(mySiDia)["BSidepo"]     <- 0.2*100          # nmol/cm2/day
sol  <- out(sim(mySiDia))
BSi  <- sol$BSi
DSi  <- sol$DSi


parms(mySiDia)["BSidepo"]     <- 2*100          # nmol/cm2/day
sol  <- out(sim(mySiDia))
BSi  <- cbind(BSi,sol$BSi)
DSi  <- cbind(DSi,sol$DSi)

parms(mySiDia)["BSidepo"]     <- 3*100          # nmol/cm2/day
sol  <- out(sim(mySiDia))
BSi  <- cbind(BSi,sol$BSi)
DSi  <- cbind(DSi,sol$DSi)

#====================#
# plotting           #
#====================#
par(mfrow=c(2,2))

Depth    <- inputs(mySiDia)$boxes$Depth
Intdepth <- inputs(mySiDia)$boxes$Intdepth
Porosity <- inputs(mySiDia)$boxes$Porosity
Db       <- inputs(mySiDia)$boxes$Db

matplot(DSi,Depth,ylim=c(10,0),xlab="mmolSi/m3 Liquid",main="DSi",type="l",lwd=c(1,2,1),col="black")
matplot(BSi,Depth,ylim=c(10,0),xlab="mmolSi/m3 Solid" ,main="BSi",type="l",lwd=c(1,2,1),col="black")
legend("right",c("0.2","2","3"),title="Depo\n mmol/m2/d",lwd=c(1,2,1),lty=1:3)
plot(Porosity,Depth,ylim=c(10,0),xlab="-" ,main="Porosity",type="l",lwd=2)
plot(Db,Intdepth,ylim=c(10,0),xlab="cm2/d" ,main="Bioturbation",type="l",lwd=2)



}
\references{
Soetaert, K and P.M.J. Herman, 2008. A practical guide to ecological Modelling. 
Using R as a simulation platform. Springer. (Chapters 3.6.5 and 7.8.5)

Schink, D.R., Guinasso, N.L., Fanning, K.A., 1975. Processes affecting the 
concentration of silica at the sediment-water interface of the Atlantic Ocean. 
Journal of Geophysical Research, 80, 3013-3031.

}

\seealso{R-package \code{\link[simecol]{simecol}} for a description of the 
  \code{\link[simecol]{simObj}} class
}

\keyword{ misc }
