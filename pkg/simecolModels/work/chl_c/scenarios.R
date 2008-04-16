## a few helper functions and data structures

unit <- list(
  p = expression(paste(oPO[4],"-P (",mu,g~L^{-1},")")),
  x = expression(paste("Phytoplankton (",mg~L^{-1},")")),
  z = expression(paste("Zooplankton (",mg~L^{-1},")")),
  n = expression(paste("N (",mg~L^{-1},")")),
  o = expression(paste(O[2]," (",mg~L^{-1},")")),
  d = expression(paste("Detritus (",mg~L^{-1},")")),
  c = expression(paste("Carbon (",mg~L^{-1},")")),
  g = expression(paste(gamma)),
  i = expression(paste("Light (",W~m^{-2},")")),
  gpp = expression(paste("spec. GPP (C/Chl-a, ",mg~mg^{-1}~d^{-1},")"))
)

reduce <- function(x, y, n = 50) {
  xx <- data.frame(x=x, y=y)
  xx <- unique(xx[order(xx$x), ])
  spline(xx, n = min(length(x), n))
}

## the simulation

chlc <- chl_c_acclimation()

times(chlc) <-  c(from=1, to=50, by=0.02)
sc1 <-sc2 <- sc3 <- chlc

## Scenario 1: strong light conditions
parms(sc1)["PAR"] <- parms(sc3)["PAR"] <- 500

## Scenario 2: low light conditions
parms(sc2)["PAR"] <- 100

## Scenario 3: constant chl:c ratio
equations(sc3)$d.gamma <- function(phox0, imean, alpha, beta,
                                gamma, gammamin, gammamax) 0

o1  <- out(sim(sc1))
o2  <- out(sim(sc2))
o3  <- out(sim(sc3))


plot(reduce(o1$time, o1$gamma, 500), type="l", ylab=unit$g, xlab="Time (d)", ylim=c(0,0.05))
lines(reduce(o2$time, o2$gamma, 500), col="blue")
lines(reduce(o3$time, o3$gamma, 10), col="red")

plot(o1$imean, o1$GPPspec, type="p", xlab=unit$i, ylab=unit$gpp, pch="+", cex=0.3)
points(o2$imean, o2$GPPspec, pch=1, col="blue", cex=0.1)
lines(reduce(x=o3$imean, y=o3$GPPspec, n=50), col="red", lwd=2, lty="dashed")

plot(reduce(o1$time, o1$carbon, 500), type="l", ylab=unit$c, xlab="Time (d)")
lines(reduce(o2$time, o2$carbon, 500), col="blue")
lines(reduce(o3$time, o3$carbon, 500), col="red")

plot(reduce(o1$time, o1$imean, 500), type="l", ylab=unit$i, xlab="Time (d)")
lines(reduce(o2$time, o2$imean, 500), col="blue")
lines(reduce(o3$time, o3$imean, 500), col="red")

plot(peaks(o1$time, o1$imean, "max"), type="l", ylab=unit$i, xlab="Time (d)")
lines(peaks(o2$time, o2$imean, "max"), col="blue")
lines(peaks(o3$time, o3$imean, "max"), col="red")
