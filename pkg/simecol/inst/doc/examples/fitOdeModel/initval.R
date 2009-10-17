## =======================================================
## Example how to fit initial values
## =======================================================

library(simecol)

## ======== load example model =========
data(chemostat)

## derive scenario
cs1 <- chemostat

## generate some noisy data
set.seed(12573) # make example reproducible
parms(cs1)[c("vm", "km")] <- c(2, 10)
times(cs1) <- c(from=0, to=20, by=2)
yobs <- out(sim(cs1))
obstime <- yobs$time
yobs$time <- NULL
yobs$S <- yobs$S + rnorm(yobs$S, sd= 0.1 * sd(yobs$S))*2
yobs$X <- yobs$X + rnorm(yobs$X, sd= 0.1 * sd(yobs$X))

## add initial values to the parameter vector
parms(cs1) <- c(parms(cs1), X=10, S=10)

## define an intifunc that copies these parameters back to init
initfunc(cs1) <- function(obj) {
  init(obj) <- parms(obj)[c("X", "S")] # Note!  Order is important!
  obj
}

## set *external* time step to same as in observations,
## and use efficient algorithm with automatic *internal* time steps
times(cs1) <- obstime
solver(cs1) <- "lsoda"

whichpar  <- c("vm", "km", "X", "S")
parms(cs1)[whichpar] <- c(vm=1, km=2, X=10, S=10)

lower <- c(vm=0, km=0, X=0, S=0)
upper <- c(vm=4, km=20, X=20, S=20)

res <- fitOdeModel(cs1, whichpar = whichpar, obstime, yobs,
  debuglevel=0, fn = ssqOdeModel,
  method = "PORT", lower = lower, upper = upper,
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

## assign fitted parameters to scenario cs1
parms(cs1)[whichpar] <- res$par

## set small external time step for good graphics and simulate again
times(cs1) <- c(from=0, to=20, by=.1)
ysim1 <- out(sim(cs1))

## compare results
par(mfrow=c(2,1))
plot(obstime, yobs$X, ylim = range(yobs$X, ysim1$X))
lines(ysim1$time, ysim1$X, col="blue")
plot(obstime, yobs$S, ylim= range(yobs$S, ysim1$S))
lines(ysim1$time, ysim1$S, col="blue")
