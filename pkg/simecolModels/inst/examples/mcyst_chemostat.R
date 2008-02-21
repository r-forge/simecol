`mcyst_chemostat` <-
function(){
  new("odeModel",
    main = function(time, init, parms) {
      x <- init
      with(as.list(parms), {
        mu  <- mu.m * x[2]/(km + x[2])
        dx1 <- mu * x[1] - D * x[1]               # microcystis
        dx2 <-  D *(S0 - x[2]) - 1/Y * mu * x[1]  # substrate
        dx3 <- mu * p * x[1] - (D + dM) * x[3]    # mcyst
        list(c(dx1, dx2, dx3))
      })
    },
    parms = c(
      mu.m = 1.08,       # max. growth rate (1/d)      (Long)
      km   = 1.61,       # half sat. constant (mumol/L) (Kohl & Nicklisch)
      Y    = 1/1.3e-3,   # Yield coefficient
      S0   = 15,         # phosphorus in import (umol/L)
      p    = 100,        # mcyst production coefficient (fg/cell)
      dM   = 0.02,       # mcyst decay coefficient (1/d) (Orr & Jones)
      D    = 0.8         # dilution rate of chemostat (1/d)
    ),
    init = c(
      cells  = 15,  # x1 in 1e6 cells/ml
      phos   = 15,  # x2 in nmol/ml
      mcyst  = 0    # x3 in ng/l
    ),
    times  = c(from=0, to=100, by=1),
    solver = "lsoda"
  )
}

