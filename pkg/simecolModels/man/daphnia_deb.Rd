\name{daphnia_deb}
\alias{daphnia_deb}
\alias{daphnia_deb_equations}

\title{Bioenergetic (Dynamic Energy Budget = DEB) Model of Daphnia}
\description{
  The DEB model is the core model of a framework for the simulation of growth
  and reproduction of Daphnia at varying conditions of food concentration and
  temperature. It is an individual level model that simulates allocation of
  assimilated carbon into somatic growth, maintenance costs, and reproduction on
  the basis of a closed carbon budget.
}

\usage{daphnia_deb()}

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
Rinke, K. and Vijverberg, J. (2005). A model approach to evaluate the effect
of temperature and food concentration on individual life-history and population
dynamics of \textit{Daphnia}. \emph{Ecological Modelling} \bold{186}, 326--344.
}


\seealso{
  \code{\link[simecol]{sim}},
  \code{\link[simecol]{parms}},
  \code{\link[simecol]{init}},
  \code{\link[simecol]{times}}.
}

\examples{
################ Basic use ########################
dap2 <- daphnia <- daphnia_deb()

parms(dap2)["kappa"] <- 0.4
inputs(dap2) <- data.frame(t.kelvin=293.15, food=5)

test  <- sim(daphnia)
test2 <- sim(dap2)

plot(test)
plot(test2)

######### Figure 2 of Rinke and Viverberg #########

## a set of different scenarios
foodconc <- c(0.1,0.25,1)     # food concentrations evaluated
temp     <- c(5,10,15,20,25)  # temperatures evaluated

dat.temp <- NULL       # data frame for storage of multiple scenarios
for(f in foodconc){    # food loop
    for(t in temp){    # temperature loop
      daphnia@inputs <- data.frame(food=f, t.kelvin=t+273.15)
      ## numerical integration
      out <- as.data.frame(out(sim(daphnia, atol=1e-4, rtol=1e-4)))
      ## preparation of outputs
      out <- cbind(out, food = f, temp = t,
                   length = equations(daphnia)$weight2length(out$weight, parms(daphnia)["l2w"])
                  )
      dat.temp <- rbind(dat.temp,out)
    }
}

## renewal of the row-names
## necessary because cbind() mangles row-names
row.names(dat.temp)<-1:nrow(dat.temp)

## write the data in an ASCII-file
# write.table(dat.temp,"dat_temp.dat", quote=F, row.names=F)


## set some plotting-related parameters
par(mfrow=c(1,3), las=1, cex.lab=1.7,
    cex.axis=1.6, cex.main=1.3, mgp=c(3.5,1,0),
    mar=c(5,5.3,4,0.2), lwd=1.5)

day <- seq(0,50,2) # scaling of the time axis
for(f in foodconc){ # food-loop
    plot(0,0,type="n", ylim=c(0,3), xlim=c(0,50),
         ylab="Length (mm)", xlab= "Time (d)", main="")
    if(f==0.1)
      title(main=expression(paste("0.1 mgC ", l^{-1})), cex.main=1.8)
    if(f==0.25)
      title(main=expression(paste("0.25 mgC ", l^{-1})), cex.main=1.8)
    if(f==1)
      title(main=expression(paste("1.0 mgC ", l^{-1})), cex.main=1.8)

    for(t in temp){ # temperature-loop
        lines(dat.temp$time[dat.temp$food==f & dat.temp$temp==t],
        dat.temp$length[dat.temp$food==f & dat.temp$temp==t],
              lty = match(t, temp))
    }
}

## draw a legend
legend(50,0,c("  5°C","10°C","15°C","20°C","25°C"),
       xjust=1, yjust=0, lwd=1.5, lty=1:5, cex=1.5, bty="n")
}
\keyword{misc}
