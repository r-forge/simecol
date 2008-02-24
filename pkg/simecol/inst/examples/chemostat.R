chemostat <- new("odeModel",
    main = function(time, init, parms, inputs=NULL) {
        vm  <- parms["vm"] # max growth rate 
        km  <- parms["km"] # half saturation constant
        D   <- parms["D"]  # dilution rate
        S0  <- parms["S0"] # substrate in inflow
        Y   <- parms["Y"]  # yield coefficient for substrate
        X   <- init[1]     # cells (e.g. algae)
        S   <- init[2]     # substrate (e.g. phosphorus)

        mu  <- vm * S/(km + S)              # Monod equation
        dx1 <- mu * X - D * X               # cells 
        dx2 <-  D *(S0 - S) - 1/Y * mu * X  # substrate
        list(c(dx1, dx2))
    },
    parms = c(
        vm = 1.0,           # 1/d
        km = 2.0,           # mumol Substrate/l
        Y  = 100,           # cells /mumol Substrate
        D  = 0.5,           # 1/d
        S0 = 10             #  mumol Substrate/l
    ),
    times = c(from=0, to=40, by=.5),
    init  = c(X=10, S=10), # cells/l; umol Substrate/l                     
    solver = "lsoda"
)

