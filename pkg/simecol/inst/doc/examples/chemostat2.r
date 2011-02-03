##==============================================================================
## Chemostat model in stoichiometry matrix formulation
##==============================================================================

chemostat2 <- new("odeModel",
  main = function(time, init, parms, inputs = NULL) {
    X <- init[1]
    S <- init[2]
    with(parms, {
      ## growth
      mu  <- vm * S/(km + S)              # Monod equation
      growth <- rep(mu * X, 2)
      
      ## dilution
      dilution <- c(-D * X, D * (S0 - S))
      dx <- N %*% growth + dilution
      list(c(dx))
    })
  },
  parms = list(
    vm = 1.0,           # max growth rate, 1/d
    km = 2.0,           # half saturation constant, mumol / L
    Y  = 100,           # cells /mumol Substrate
    D  = 0.5,           # dilution rate, 1/d
    S0 = 10,            # substrate in inflow, mumol / L
    N  = matrix(c(0, 1, 0, -1/100), nrow=2, byrow=TRUE)

  ),
  times = c(from=0, to=40, by=.5),
  init  = c(X=10, S=10), # cells / L; Substrate umol / L
  solver = "lsoda",
  initfunc = function(obj) {
    Y <- parms(obj)$Y
    parms(obj)$N <-
      matrix(c(0, 1,
            -1/Y, 0), nrow=2, byrow=TRUE) # stoichiometry matrix
    obj
  }
)

plot(sim(chemostat2))