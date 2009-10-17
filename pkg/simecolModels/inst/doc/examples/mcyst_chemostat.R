`mcyst_chemostat` <-
function(){
  new("odeModel",
    main = function(time, init, parms) {
      with(as.list(c(init, parms)), {
        mu  <- mu.m * phos/(km + phos)
        dcells <- mu * cells - D * cells               # microcystis
        dphos  <-  D *(S0 - phos) - 1/Y * mu * cells   # substrate
        dmcyst <- mu * p * cells - (D + dM) * mcyst    # mcyst
        list(c(dcells, dphos, dmcyst))
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

