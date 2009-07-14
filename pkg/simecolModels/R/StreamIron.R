##########################################################################
#
#   1986 St. Kevin Steady-State Iron (Kimball et al., 1991)
#
#   Lateral inflow, transport and conumption of iron in St. Kevin Gulch,
#   a 1904 m long headwater stream in the Rocky Mountains
#
#   as implemented in the OTIS modelling framework - application 6
#
##########################################################################

#==============================================================================
# River spiral model with lateral input
#==============================================================================

StreamIron <- function() {
  new("odeModel",
    main = function(t,state,pars) {
      with (as.list(c(pars, inputs$boxes)),{
      Fe    <- state[1        :Nb    ]       # concentration in main channel
      Fe_s   <- state[(Nb+1)   :(2*Nb)]      # concentration in storage zone

# transport - only Fe is transported
      tranC <- tran.volume.1D(C=Fe, C.up=Cup, flow=Q,
                            flow.lat=Qlat, C.lat=Clat, Disp=D, V=Vol)

# exchange between the two phases
      exch <- Alpha * (Fe_s - Fe)

# the rate of change
      dFe    <- tranC$dC - Lam*Fe   + exch
      dFe_s  <-                     - exch * A/As

      return(list(c(dFe,dFe_s)))
      })
    },          

    # the model parameters:  
    parms = c(Q    = 6.12e-3,      # m3/sec, flow rate
              Cup  = 0.64         # upstream concentration

    ),
    times = 0,       # steady-state solution

    # this model comes with a user defined solver,
    #   i.e. a function instead of a character that points to an existing solver

    solver = function(y, times, func, parms, ...) {
      ST <- steady.1D(y, time=0, func = func, nspec = 2, parms=parms,
          pos=TRUE, names=c("Fe", "Fe_s"))

      ST$y
    },

    # derived and additional inputs which require internal calculations,
    # initfunc is called automatically during object creation
    initfunc = function(obj) {
     pars <- parms(obj)
      with(as.list(pars),{

        # box size, metres
        dx <- 1

# 7 segments, with different parameter values
        riverlen <- c(     26,    337,     121,      42,     422,     609,     347)
        area     <- c(   0.12 , 0.097,   0.097,   0.138,   0.199,   0.152,   0.153)
        area2    <- c(   0.05,   0.05,    0.05,    0.25,    0.10,    0.20,    0.20)
        alpha    <- c( 3.0e-5, 2.0e-5,  2.0e-5,  2.0e-5,  1.5e-5,    5e-5,    3e-5)
        qlat     <- c(      0,3.78e-6, 3.78e-6,  1.7e-4, 4.12e-6, 4.84e-6,-2.03e-5)
        clat     <- c(   0.56,   0.56,   211.0,    0.56,    0.56,    0.56,    0.56)
        lam      <- c(1.26e-3,3.74e-4, 1.26e-4, 2.45e-4, 1.12e-4, 1.02e-4,    1e-4)

# Distances (x), and position of each box in river segment (ix)
        x  <- seq(from=dx/2, to = sum(riverlen), by = dx)
        Nb <- length (x)         # total number of boxes

        ix <- NULL
        for ( i in 1:length(riverlen)) ix <- c(ix, rep(i,riverlen[i]/dx))

# Values of parameters in each box
        Disp  <- 0.02            # m2/sec
        A     <- area[ix]        # m2
        As    <- area2[ix]       # m2
        Alpha <- alpha[ix]       # /sec
        Qlat  <- qlat[ix]        # m3/s, lateral flow rate
        Clat  <- clat[ix]        # m3/s, lateral flow rate
        D     <- Disp*A/dx       # m3/s, bulk dispersion coefficient
        Vol   <- A*dx            # m3  , river volume
        Lam   <- lam[ix]         # /s

        init(obj) = rep(0.64,2*Nb)

        inputs(obj)$boxes <- list(x=x, Disp = Disp, A = A, As = As, Alpha = Alpha,
          Lam = Lam, Qlat=Qlat, Clat = Clat, D=D, Vol=Vol, Nb=Nb)
        return(obj)
      })
    }
  )
}
