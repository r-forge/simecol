###################################################
### chunk number 1: init
###################################################
library("simecol")
data(lv, package = "simecol")
options("width"=72)
options("prompt" = "R> ", "continue" = "+  ")


###################################################
### chunk number 2: upca1
###################################################
f <- function(x, y, k){x*y / (1+k*x)}  # Holling II

func <- function(time, y, parms) {
  with(as.list(c(parms, y)), {
    du <-  a * u           - alpha1 * f(u, v, k1)
    dv <- -b * v           + alpha1 * f(u, v, k1) +
                           - alpha2 * f(v, w, k2)
    dw <- -c * (w - wstar) + alpha2 * f(v, w, k2)
    list(c(du, dv, dw))
  })
}

times  <- seq(0, 100, 0.1)
parms  <- c(a=1, b=1, c=10, alpha1=0.2, alpha2=1, k1=0.05, k2=0, wstar=0.006)
y      <- c(u=10, v=5, w=0.1)


###################################################
### chunk number 3: upca2
###################################################
library(deSolve)
out <- lsoda(y, times, func, parms)
matplot(out[,1], out[,-1], type="l")


###################################################
### chunk number 4: upca3
###################################################
library("simecol")
f <- function(x, y, k){x*y / (1+k*x)}  # Holling II

upca <- new("odeModel",
  main = function(time, y, parms) {
    with(as.list(c(parms, y)), {
      du <-  a * u           - alpha1 * f(u, v, k1)
      dv <- -b * v           + alpha1 * f(u, v, k1) +
                             - alpha2 * f(v, w, k2)
      dw <- -c * (w - wstar) + alpha2 * f(v, w, k2)
      list(c(du, dv, dw))
    })
  },
  times  = seq(0, 100, 0.1),
  parms  = c(a=1, b=1, c=10, alpha1=0.2, alpha2=1,
    k1=0.05, k2=0, wstar=0.006),
  init   = c(u=10, v=5, w=0.1),
  solver = "lsoda"
)


###################################################
### chunk number 5: upca4
###################################################
upca <- sim(upca)
plot(upca)


###################################################
### chunk number 6: upca5
###################################################
plotupca <- function(obj, ...) {
  o <- out(obj)
  matplot(o[,1], o[,-1], type="l", ...)
  legend("topright", legend = c("u", "v", "w"), lty=1:3, , bg="white",col = 1:3)
}
plotupca(upca)


###################################################
### chunk number 7: upca6
###################################################
sc1 <- sc2 <- upca
parms(sc1)["wstar"] <- 0
parms(sc2)["wstar"] <- 0.1
sc1 <- sim(sc1)
sc2 <- sim(sc2)
par(mfrow=c(1,2))
plotupca(sc1, ylim=c(0, 250))
plotupca(sc2, ylim=c(0, 250))


###################################################
### chunk number 8: upca7
###################################################
f <- function(x, y, k){x * y}


###################################################
### chunk number 9: upca8
###################################################
sc1 <- sim(sc1)
sc2 <- sim(sc2)
par(mfrow=c(1,2))
plotupca(sc1, ylim=c(0, 20))
plotupca(sc2, ylim=c(0, 20))


###################################################
### chunk number 10: upca9
###################################################
sc1 <- sc2 <- upca
equations(sc1)$f <- function(x, y, k){x*y / (1+k*x)}
equations(sc2)$f <- function(x, y, k){x * y}
sc1 <- sim(sc1)
sc2 <- sim(sc2)
par(mfrow=c(1,2))
plotupca(sc1, ylim=c(0, 20))
plotupca(sc2, ylim=c(0, 20))


###################################################
### chunk number 11: upca10
###################################################
upca <- new("odeModel",
  main = function(time, y, parms) {
    with(as.list(c(parms, y)), {
      du <-  a * u           - alpha1 * f(u, v, k1)
      dv <- -b * v           + alpha1 * f(u, v, k1) +
                             - alpha2 * f(v, w, k2)
      dw <- -c * (w - wstar) + alpha2 * f(v, w, k2)
      list(c(du, dv, dw))
    })
  },
  equations  = list(
    f1 = function(x, y, k){x*y},           # Lotka-Volterra
    f2 = function(x, y, k){x*y / (1+k*x)}  # Holling II
  ),
  times  = seq(0, 100, 0.1),
  parms  = c(a=1, b=1, c=10, alpha1=0.2, alpha2=1, k1=0.05, k2=0, wstar=0.006),
  init   = c(u=10, v=5, w=0.1),
  solver = "lsoda"
)

equations(upca)$f <- equations(upca)$f1


###################################################
### chunk number 12: upca11
###################################################
f <- function(x, y, k){x*y / (1+k*x)}  # Holling II

fmain <-  function(time, y, parms) {
  with(as.list(c(parms, y)), {
    du <-  a * u           - alpha1 * f(u, v, k1)
    dv <- -b * v           + alpha1 * f(u, v, k1) +
                           - alpha2 * f(v, w, k2)
    dw <- -c * (w - wstar) + alpha2 * f(v, w, k2)
    list(c(du, dv, dw))
  })
}

upca <- new("odeModel",
  main = function(time, y, parms) fmain(time, y, parms),
  times  = seq(0, 100, 0.1),
  parms  = c(a=1, b=1, c=10, alpha1=0.2, alpha2=1, k1=0.05, k2=0, wstar=0.006),
  init   = c(u=10, v=5, w=0.1),
  solver = "lsoda"
)


###################################################
### chunk number 13:  eval=FALSE
###################################################
## debug(fmain)
## upca <- sim(upca)


###################################################
### chunk number 14: upca12
###################################################
main(upca)        <- fmain   # assign workspace function to main slot
equations(upca)$f <- f       # assign workspace function to equations
rm(fmain, f)   # optional, for saving memory and avoiding confusion
str(upca)      # show the object


###################################################
### chunk number 15: upca13
###################################################
save(upca, file="upca.Rdata")  # persistent storage of the model object
load("upca.Rdata")             # load the model


###################################################
### chunk number 16: 
###################################################
l.upca <- as.list(upca)


###################################################
### chunk number 17: 
###################################################
dput(l.upca, file="upca_list.R")


###################################################
### chunk number 18: 
###################################################
l.upca <- dget("upca_list.R")
upca <- as.simObj(l.upca)


###################################################
### chunk number 19: lvgen
###################################################
genLV <- function() {
  new("odeModel",
    main = function (time, init, parms) {
      x <- init
      p <- parms
      dx1 <-   p["k1"] * x[1] - p["k2"] * x[1] * x[2]
      dx2 <- - p["k3"] * x[2] + p["k2"] * x[1] * x[2]
      list(c(dx1, dx2))
    },
    parms  = c(k1=0.2, k2=0.2, k3=0.2),
    times  = c(from=0, to=100, by=0.5),
    init   = c(prey=0.5, predator=1),
    solver = "lsoda"
  )
}


###################################################
### chunk number 20: lvgen2
###################################################
lv1 <- genLV()
plot(sim(lv1))


###################################################
### chunk number 21: showMethods
###################################################
showMethods("sim")


###################################################
### chunk number 22: getMethod
###################################################
getMethod("sim", "odeModel")


###################################################
### chunk number 23: 
###################################################
data(chemostat)
cs1 <- cs2 <- chemostat


###################################################
### chunk number 24: 
###################################################
obstime <- seq(0, 20, 2)
yobs <- data.frame(
  X    = c(10, 26, 120, 197, 354, 577, 628, 661, 654, 608, 642),
  S    = c(9.6, 10.2, 9.5, 8.2, 6.4, 4.9, 4.2, 3.8, 2.5, 3.8, 3.9)
)


###################################################
### chunk number 25: 
###################################################
times(cs1)  <- obstime
solver(cs1) <- "lsoda"


###################################################
### chunk number 26:  eval=FALSE
###################################################
## res <- fitOdeModel(cs1, obstime = obstime, yobs=yobs)


###################################################
### chunk number 27: 
###################################################
whichpar <- c("vm", "km", "Y")
lower    <- c(vm=0, km=0, Y=0)
upper    <- c(vm=100, km=500, Y=200)
parms(cs1)[whichpar] <- c(vm=5, km=10, Y=100)

res <- fitOdeModel(cs1, whichpar = whichpar,
  lower = lower, upper=upper,
  obstime = obstime, yobs = yobs, method = "PORT",
  control=list(trace = FALSE))


###################################################
### chunk number 28: 
###################################################
res


###################################################
### chunk number 29: 
###################################################
parms(cs2)[whichpar] <- res$par


###################################################
### chunk number 30: 
###################################################
times(cs2) <- obstime
ysim <- out(sim(cs2))
1 - var(ysim$X - yobs$X) / var(yobs$X)
1 - var(ysim$S - yobs$S) / var(yobs$S)


###################################################
### chunk number 31: fit1
###################################################
plotFit <- function() {
  solver(cs2) <- "lsoda"
  times(cs2) <- c(from=0, to=20, by=.1)
  ysim <- out(sim(cs2))
  par(mfrow=c(1,2))
  plot(obstime, yobs$X, ylim = range(yobs$X, ysim$X))
  lines(ysim$time, ysim$X, col="red")
  plot(obstime, yobs$S, ylim= range(yobs$S, ysim$S))
  lines(ysim$time, ysim$S, col="red")
}
plotFit()


###################################################
### chunk number 32: 
###################################################
parms(cs1) <- c(parms(cs1), init(cs1))
parms(cs1)


###################################################
### chunk number 33: 
###################################################
initfunc(cs1) <- function(obj) {
  init(obj) <- parms(obj)[c("X", "S")]
  obj
}


###################################################
### chunk number 34: 
###################################################
whichpar <- c("vm", "km", "X", "S")
lower    <- c(vm=0, km=0, X=0, S=0)
upper    <- c(vm=100, km=500, X=100, S=100)
parms(cs1)[whichpar] <- c(vm=10, km=10, X=10, S=10)

res <- fitOdeModel(cs1, whichpar = whichpar,
  lower = lower, upper=upper,
  obstime = obstime, yobs = yobs, method = "Nelder",
  control=list(trace = FALSE))


###################################################
### chunk number 35: fit2
###################################################
initfunc(cs2) <- initfunc(cs1)
parms(cs2)[whichpar] <- res$par

plotFit()


###################################################
### chunk number 36: fit3
###################################################
whichpar <- c("vm", "km")
parms(cs1)[whichpar] <- c(vm=10, km=10)

res <- fitOdeModel(cs1, whichpar = whichpar,
  obstime = obstime, yobs = yobs, method = "Nelder")
res$value
res$par


###################################################
### chunk number 37: 
###################################################
res <- fitOdeModel(cs1, whichpar = whichpar,
  obstime = obstime, yobs = yobs, method = "Nelder",
  sd.yobs=c(1,1))

res$value
res$par


###################################################
### chunk number 38: 
###################################################
weights <- data.frame(X = rep(1, nrow(yobs)),
                      S = rep(1, nrow(yobs)))
weights[9:11,] <- 1/3
res <- fitOdeModel(cs1, whichpar = whichpar,
  obstime = obstime, yobs = yobs, method = "Nelder",
  weights = weights)

res$value
res$par


###################################################
### chunk number 39: 
###################################################
res <- fitOdeModel(cs1, whichpar = whichpar,
  obstime = obstime, yobs = yobs, method = "PORT",
  scale = 1/c(vm=1, km=10))

res$value
res$par


###################################################
### chunk number 41: 
###################################################
library(FME)
library(simecol)
data(chemostat)

cs1 <- chemostat
solver(cs1) <- "lsoda"

obstime <- seq(0, 20, 2)
yobs <- data.frame(
  X    = c(10, 26, 120, 197, 354, 577, 628, 661, 654, 608, 642),
  S    = c(9.6, 10.2, 9.5, 8.2, 6.4, 4.9, 4.2, 3.8, 2.5, 3.8, 3.9)
)

Cost <- function(p, simObj, obstime, yobs) {
  whichpar <- names(p)
  parms(simObj)[whichpar] <- p
  times(simObj) <- obstime
  ysim <- out(sim(simObj))
  modCost(ysim, yobs, weight="std")
}

yobs <- cbind(time=obstime, yobs)

Fit <- modFit(p = c(vm=10, km=10), f = Cost, simObj=cs1,
  obstime=obstime, yobs=yobs, method="Nelder", control=list(trace=FALSE))


###################################################
### chunk number 42: 
###################################################
summary(Fit)
deviance(Fit)
coef(Fit)


###################################################
### chunk number 43:  eval=FALSE
###################################################
## residuals(Fit)
## df.residual(Fit)
## plot(Fit)


###################################################
### chunk number 44: compile.clotka eval=FALSE
###################################################
## system("R CMD SHLIB clotka.c")


###################################################
### chunk number 45: clotka_R eval=FALSE
###################################################
## modeldll <- dyn.load("clotka.dll")


###################################################
### chunk number 46: cleanup
###################################################
options("prompt" = "> ", "continue" = "+ ")


