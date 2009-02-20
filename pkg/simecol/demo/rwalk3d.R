################################################################################
## Demo rwalk3d
##   - demonstrates random walk models in 3D
##   - different movement rules
##       - classical rectangular
##       - circular (brownian motion)
##   - plot function for illustrating the trajectories in 3D
##   - observer function to implement user-defined animation
##
## Note: This demo may be changed in the future:
##   - lists2matrix and trajectories may become package functions,
##     maybe with slightly different syntax
##   - rwalk3d will become an example model of simecol
################################################################################
opar   <- par(no.readonly=TRUE)
oask   <- devAskNewPage(dev.interactive(orNone = TRUE))
defpar <- par(no.readonly = TRUE)

library(simecol)

trajectories3d <- function(x, method = c("scatterplot3d", "rgl"), ...) {
  lists2matrix <- function(o) {
    m <- o[[1]]
    m <- cbind(1:nrow(m), 1, m)
    for (i in 2:length(o)) {
      m1 <- o[[i]]
      m1 <- cbind(1:nrow(m1), i, m1)
      m <- rbind(m, m1)
    }
    colnames(m) <- c("id", "time", "x", "y", "z", "phi", "theta")
    m
  }
  method = match.arg(method)
  o <- out(x)
  dat  <- as.data.frame(lists2matrix(o))
  n <- max(dat$id)
  cols <- rainbow(n)
  dat1 <- subset(dat, id==1)
  if (method == "rgl") {
    if (!require("rgl")) stop("Package rgl is not installed")
    plot3d(dat1$x, dat1$y, dat1$z, type="l",
      col=cols[1], xlim=range(dat$x), ylim=range(dat$y), zlim=range(dat$z),
      xlab="x", ylab="y", zlab="z", ...)
    for (i in 2:n) {
      dat1 <- subset(dat, id==i)
      lines3d(dat1$x, dat1$y, dat1$z, col=cols[i])
    }
  } else {
    if (!require("scatterplot3d")) stop("Package scatterplot3d is not installed")
    s3d  <- scatterplot3d(dat1$x, dat1$y, dat1$z, type="l",
      color=cols[1], xlim=range(dat$x), ylim=range(dat$y), zlim=range(dat$z),
      xlab="x", ylab="y", zlab="z", ...)
    for (i in 2:n) {
      dat1 <- subset(dat, id==i)
      s3d$points3d(dat1$x, dat1$y, dat1$z, type="l", col=cols[i])
    }
  }
}

## =============================================================================

rwalk3d <- new("rwalkModel",
  main = function(time, init, parms) {
    init <- move(init, nrow(init), parms$delta, parms$turn)
    if (parms$wraparound) init <- wrap3(init, parms)
    init
  },
  equations = list(
    rectangular = function(x, ninds, delta, ...) {
      ## 3 columns with zeros
      dxyz <- matrix(0, nrow = ninds, ncol = 3)
      ## one column with random 1:3
      d    <- sample(1:3, ninds, replace = TRUE) * delta
      ## cbind: row_index, col_index --> 3 cols with exactly one 1, -1 per row
      dxyz[cbind(1:ninds, d)] <- sample(c(-1, 1), ninds, replace = TRUE)
      x[,1:3] <- x[,1:3]  + dxyz
      x[,4:5] <- 0 # an 'observer' can help to avoid this
      x
    },
    circular = function(x, ninds, delta, turn) {
      phi    <- (x[,4] - turn/2 + turn/2 * pi / runif(ninds)) %% (2 * pi)
      theta  <- (x[,5] - turn/2 + turn/2 * pi / runif(ninds)) %% (2 * pi)
      dx <- matrix(0, nrow=ninds, ncol = 3)
      dx[, 1] <- delta * sin(theta) * cos(phi)
      dx[, 2] <- delta * sin(theta) * sin(phi)
      dx[, 3] <- delta * cos(theta)
      x[, 1:3] <- x[, 1:3] + dx
      x[, 4] <- phi
      x[, 5] <- theta
      x
    },
    wrap3 = function(init, parms) {
      init[,1] <- wrap(init[,1], parms$area[1], parms$area[2])
      init[,2] <- wrap(init[,2], parms$area[3], parms$area[4])
      init[,3] <- wrap(init[,3], parms$area[5], parms$area[6])
      init
    },
    wrap = function(x, xmin, xmax) {
      x <- ifelse(x < xmin, xmax - (xmin - x), x)
      x <- ifelse(x > xmax, xmin + (x - xmax), x)
      x
    }
  ),
  times  = c(from=0, to=100, by=1),
  parms  = list(ninds = 100, delta = 1, turn = 1, wraparound = FALSE,
             area = c(-30, 30, -30, 30, -30, 30)),
  solver = "iteration",
  initfunc = function(obj){
    ninds <- parms(obj)$ninds
    init = matrix(0, nrow=ninds, ncol = 5,
             dimnames = list(NULL, c("x", "y", "z", "delta", "theta")))
    init[,4] <- runif(ninds) * 2 * pi
    init[,5] <- runif(ninds) * 2 * pi
    obj@init <- init
    obj
  }
)

##==============================================================================

## set the movement rule to one of the two pre-defined functions
equations(rwalk3d)$move <- equations(rwalk3d)$rectangular

## simulate the model and do a little bit statistics
par(defpar) # interactive on

rwalk3d <- sim(rwalk3d, animate=TRUE)
o <- as.data.frame(out(rwalk3d, last=TRUE))

par(opar) # interactive off (for animation)

hist(o$x)
var(o$x) + var(o$y) + var(o$z)                      # estimated variance
(parms(rwalk3d)$delta)^2 * times(rwalk3d)["to"]     # expected variance

par(defpar) # interactive on
x11()

## change the movement rule to the other alternative
equations(rwalk3d)$move <- equations(rwalk3d)$circular
parms(rwalk3d)$ninds <- 100
parms(rwalk3d)$turn <- .1
rwalk3d <- initialize(rwalk3d)
rwalk3d <- sim(rwalk3d)

par(opar) # interactive off (for animation)

## plot the trajectories
trajectories3d(rwalk3d)
trajectories3d(rwalk3d, method="rgl")

par(defpar) # interactive on

## assign an observer function to the model for 3D visualization at run-time
observer(rwalk3d) <- function(x, ...) {
  cols   <- rainbow(nrow(x))
  shapes <- rgl.ids()$id
  if (!length(shapes)) { # first call
    rgl.open()
    rgl.bg(color="black")
  } else {
    par3d(skipRedraw = TRUE)
    rgl.pop(type = "shapes", shapes)
  }
  limits <- c(-33,33)
  points3d(expand.grid(limits, limits, limits), col="black")
  spheres3d(x[, 1:3], col=cols, radius =2)
  par3d(skipRedraw=FALSE)
  x  # observer must return something to save, (default: state x)
}

## set a reasonable number of time steps and individuals and simulate the model
times(rwalk3d) <- 1:250
parms(rwalk3d)$wraparound <- TRUE
parms(rwalk3d)$ninds <- 50
rwalk3d <- initialize(rwalk3d)
rwalk3d <- sim(rwalk3d)

## clean up
par(defpar)
devAskNewPage(oask)
