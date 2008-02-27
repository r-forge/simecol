library(simecol)
data(chemostat)


cs1 <- cs2 <- chemostat

## generate some noisy data
parms(cs1)[c("vm", "km")] <- c(2, 10)
times(cs1) <- c(from=0, to=20, by=2)
yobs <- out(sim(cs1))
obstime <- yobs$time
yobs$time <- NULL
yobs$S <- yobs$S + rnorm(yobs$S, sd= 0.1 * sd(yobs$S))*2
yobs$X <- yobs$X + rnorm(yobs$X, sd= 0.1 * sd(yobs$X))



## optimize it!

## time steps for simulation, either small for rk4 fixed step
# times(cs2)["by"] <- 0.1
# solver(cs2) <- "rk4"

## or, faster: only required steps for solver with automatic step size
times(cs2) <- obstime
solver(cs2) <- "lsoda"

## Nelder-Mead (default)
whichpar  <- c("vm", "km")

res <- fitOdeModel(cs2, whichpar=whichpar, obstime, yobs, 
  debuglevel=0,
  control=list(trace=TRUE))

parms(cs2)[whichpar] <- res$par 

## alternatively, L-BFGS-B (allows lower and upper bounds for parameters)
res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs, 
  debuglevel=0, fn = ssqOdeModel,
  method = "L-BFGS-B", lower = 0,
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

## set parameters of fitted model to 
parms(cs2)[whichpar] <- res$par 

times(cs2) <- c(from=0, to=20, by=1)
ysim <- out(sim(cs2))

par(mfrow=c(2,1))

plot(obstime, yobs$X, ylim = range(yobs$X, ysim$X))
lines(ysim$time, ysim$X, col="red")

plot(obstime, yobs$S, ylim= range(yobs$S, ysim$S))
lines(ysim$time, ysim$S, col="red")

