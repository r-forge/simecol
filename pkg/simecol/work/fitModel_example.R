library(simecol)
data(chemostat)


cs1 <- cs2 <- chemostat

## generate some noisy data
parms(cs1)[c("vm", "km")] <- c(2, 10)
times(cs1) <- c(from=0, to=30, by=5)
yobs <- out(sim(cs1))
time <- yobs$time
yobs$time <- NULL
yobs$S <- yobs$S + rnorm(yobs$S, sd= 0.1 * sd(yobs$S))*2
yobs$X <- yobs$X + rnorm(yobs$X, sd= 0.1 * sd(yobs$X))



times(cs2) <- time

## optimize it!

## Nelder-Mead (default)
whichpar  <- c("vm", "km")

res <- fitOdeModel(cs2, whichpar=whichpar, time, yobs, 
  debuglevel=0,
  control=list(trace=TRUE))

parms(cs2)[whichpar] <- res$par 

## alternatively
res <- fitOdeModel(cs2, whichpar=c("vm", "km"), time, yobs, 
  debuglevel=0, fn = ssqOdeModel,
  method = "L-BFGS-B", lower = 0,
  control=list(trace=TRUE),
  atol=1e-4, rtol=1e-4)

parms(cs2)[whichpar] <- res$par 

times(cs2) <- c(from=0, to=30, by=1)
ysim <- out(sim(cs2))

par(mfrow=c(2,1))

plot(time, yobs$X)
lines(ysim$time, ysim$X, col="red")

plot(time, yobs$S)
lines(ysim$time, ysim$S, col="red")

