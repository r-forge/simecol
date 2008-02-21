## Second part of the DVM-Daphnia model  (DVM=diurnal vertical migration)
##
## Beware:
## the movement rules are far from realism 
## and intended for teaching and demonstration ob basic principles only

library(simecolModels)

source("daphnia_ibm_dvm.R")

m <- daphnia_ibm_dvm()
m <- initialize(m)

equations(m)$move <- function(time, init, parms) {
  init
  n    <- nrow(init)
  hour <- (time - floor(time)) * 24
  z    <- init$z
  dvm  <- 0
  if ((hour > 22) & (hour < 23)) {
    dvm <- (parms$ndepth - z) * rweibull(n, shape=2, scale=40)
    cat("up\n")
  }
  if ((hour > 5) & (hour < 6)) {
    dvm <- (parms$ddepth - z) * rweibull(n, shape=2, scale=40)
    cat("down\n")
  }
  dvm <- ifelse (init$age > parms$SAM, dvm, 0)
  z <- z + (dvm + rnorm(n, mean = -5, sd = 40)) * parms$DELTAT
  z <- ifelse(z < 0, 1, z)
  z <- ifelse(z > 40, 38.8, z)
  init$z <- z
  init
}

equations(m)$live = function(inds, parms){
  temp <- function(z, zmix = 10, k= -0.2) {
    ifelse(z < zmix, 20, 4 + 16 * exp(k * (z - zmix)))
  }
  food <- function(z) {
    dnorm(z, mean=5, sd=2) + 0.2 * dnorm(z, mean=15,sd=1)
  }
  with(parms, {
    ninds       <- nrow(inds)
    inds$age    <- inds$age + DELTAT
    inds$eggage <- ifelse(inds$size > SAM & inds$eggs > 0,
                          inds$eggage + DELTAT, 0)
    tefi_out    <- tefi(inds$age, temp(inds$z), food(inds$z), parms)
    inds$size   <- tefi_out$L
    neweggs     <- round(tefi_out$E)
    inds$eggs   <- ifelse(inds$size > SAM & inds$eggage==0,
                          neweggs, inds$eggs)
    inds
  })
}

silentObserver <- function(init) {
  n <- nrow(init)
  pal <- c("red", "orange", "green", "blue", "navy")
  age <- init$age
  c(n=n) # Number of individuals
}

visualObserver <- function(init) {
  n <- nrow(init)
  cat(n, "\n")
  pal <- c("red", "orange", "green", "blue", "navy")
  age <- init$age
  plot(1:nrow(init), init$z, ylim=c(40, 0), xlim=c(0, pmax(500, n)),
    pch=".", cex=2, xlab="Individuals", ylab="Depth (m)",
    col=pal[1+ (age > 2) + (age > 5) + (age > 10) + (age > 20)])
  #init # <-- attention, everything is remembered during the simulation
  c(n=n) # Number of individuals
}


parms(m)$mort <- 0.15
parms(m)$maxage <- 60

parms(m)$ndepth <- 4   # favorite depth at night
parms(m)$ddepth <- 35  # favorite depth at day

observer(m) <- visualObserver    # real-time visualization
# observer(m) <- silentObserver  # faster computation


times(m) <- c(by=0.007, to = 40)
m <- sim(m)

o <- out(m)

plot(o, type="l")

