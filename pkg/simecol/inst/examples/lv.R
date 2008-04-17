########################################################################
## Predator-Prey Model
########################################################################

lv <- new("odeModel", 
  main = function (time, init, parms) {
    x <- init
    p <- parms
    dx1 <-   p["k1"] * x[1] - p["k2"] * x[1] * x[2]
    dx2 <- - p["k3"] * x[2] + p["k2"] * x[1] * x[2]
    list(c(dx1, dx2))
  },
  parms  = c(k1=0.2, k2=0.2, k3=0.2),
  #times  = seq(0, 100, 0.5),
  times  = c(from=0, to=100, by=0.5),
  init   = c(prey=0.5, predator=1),
  solver = "rk4"
)


