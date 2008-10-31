\name{mcyst_batch_mc}

\alias{mcyst_batch_mc}
\title{Microcystin Production Model (Batch Version for Monte Carlo Simulations)}
\description{
  This is the batch version of the model described by Jähnichen et. al (submitted).
}
\usage{
  mcyst_batch_mc()
}

\value{S4 object according to the \code{\link[simecol]{odeModel}} specification.
The object contains the following slots:
  %\describe{
    \item{main}{The differential equations for cell abundance and
      Microcystin concentration:
    \describe{
      \item{cells}{abundance of cells (1/L),}
      \item{mcyst}{microcystin concentration (fg/L).}
    }
  }
  \item{parms}{Vector with the named parameters of the model:
    \describe{
      \item{mu}{Growth rate of cells (1/d),}
      \item{K}{Carrying Capacity (cells /L),}
      \item{dMmin}{minimal value of MCYST depletion rate (1/d),}
      \item{dMmax}{maximal value of MCYST depletion rate (1/d),}
      \item{dMrho}{autocorrelation coefficient for MCYST decay rate,}
      \item{pmin}{minimal value of MCYST production coefficient (fg/cell),}
      \item{pmax}{minimal value of MCYST production coefficient (fg/cell),}
      \item{prho}{autocorrelation coefficient for MCYST production.}
    }  
  }
  \item{inputs}{matrix with time-dependend signals. The two parameters
    \code{dM} and \code{p} are considered as stochastic, time dependend 
    signals in the Monte Carlo version of the MCYST production model.
    \describe{
      \item{x}{time (d),}
      \item{p}{MCYST production coefficient (fg/cell),}
      \item{dM}{MCYST depletion rate (1/d)}
    }
  }    
  \item{times}{Simulation time and integration interval.}
  \item{init}{Vector with start values for \code{cells} and \code{mcyst}.}
  \item{solver}{Character string with the integration method.}
}

\details{
  To see all details, please have a look into the implementation and the
  original publications.
}
\references{
  Jähnichen, S., Ihle, T. and Petzoldt, T. (2008).
  Variability of microcystin cell quota: A small model explains dynamics and
  equilibria. \emph{Limnologica - Ecology and Management of Inland Waters},
  \bold{38}, 339-349. URL \url{http://dx.doi.org/10.1016/j.limno.2008.05.003}
}

\seealso{
  \code{\link[simecol]{sim}},
  \code{\link[simecol]{parms}},
  \code{\link[simecol:parms]{init}},
  \code{\link[simecol:parms]{times}}.
}
\examples{
  mcystmod <- mcyst_batch_mc()
  ## sim initializes automatically and draws new random numbers for the signals
  plot(sim(mcystmod))

  ## derive a scenario

  parms(mcystmod)["prho"] <- 0.7
  parms(mcystmod)["dMmin"] <- 0.02
  parms(mcystmod)["dMmax"] <- 0.02
  
  ## Monte Carlo Simulation
  for (i in 1:10) { # number of runs, increase this to > 1000 for real simulations
     out <- out(sim(mcystmod))
    if (i==1) allout <- out else allout <- rbind(allout, out)
  }
  
  mu_cells <- diff(log(allout$cells))/diff(allout$time) # spezific growth rate
  mu_mcyst <- diff(log(allout$mcyst))/diff(allout$time) # spezific MCYST-production
  q_mcyst <- allout$mcyst/allout$cells
  
  mu <- parms(mcystmod)["mu"]
  K  <- parms(mcystmod)["K"]
  
  deriv <- mu * allout$cells*(1-allout$cells/K)
  par(mfrow=c(3,3))
  
  plot(allout$time, allout$cells, col="green", type="p",
    xlab="time", ylab="Cells", ylim=c(0, max(out$cells)))
  plot(allout$time, allout$mcyst*1e-6, col="red", type="p",
    xlab="time", ylab="MCYST", ylim=c(0, max(out$mcyst*1e-6)))
  plot(allout$time, q_mcyst, col="red",  type="p",
    xlab="time", ylab="Q_MCYST")
  plot(mu_cells, mu_mcyst,col="red",type="p",
    xlab="mu_Zellen", ylab="mu_MCYST",ylim=c(-0,0.5))
  
  plot(allout$time[-1],mu_cells,col="blue",type="p",
    xlab="time", ylab="mu_cells")
  plot(allout$time[-1],mu_mcyst,col="blue",type="p",
    xlab="time", ylab="mu_MCYST", ylim=c(0,0.5))
  plot(allout$time[-1],mu_mcyst/mu_cells,col="blue",type="p",
    xlab="time", ylab="mu_MCYST/mu_cells", ylim=c(0.8,1.2))
  plot(allout$time,deriv, col="blue",type="p",
    xlab="time", ylab="dx.dt" )
  
  ## derive some numeric results
  results1 <- data.frame(allout$time,
                         allout$cells,
                         allout$mcyst, q_mcyst)
  results2 <- data.frame(mu_cells, mu_mcyst, mu_mcyst/mu_cells)
  
  qmax  <- aggregate(allout$mcyst/allout$cells,list(allout$time),max)
  qmin  <- aggregate(allout$mcyst/allout$cells,list(allout$time),min)
  qmean <- aggregate(allout$mcyst/allout$cells,list(allout$time),mean)
  qsd   <- aggregate(allout$mcyst/allout$cells,list(allout$time),sd)
  
  mumean<-aggregate(diff(log(allout$cells))/
                    diff(allout$time),list(allout$time[-1]),mean)

}

\keyword{ misc }
