##########################################################################
#
#   1972 Uvas Creek Tracer Injection
#   Transport of Strontium AND chloride.
#
#   Parameter Values from:
#
#   Strontium:
#   Bencala, K.E., 1983, Simulation of solute transport in a mountain
#   pool-and-riffle stream with a kinetic mass transfer model for sorption:
#   Water Resources Research, v. 19, no. 3, p. 732-738.
#
#   Chloride
#   Bencala, K.E. and R.A. Walters, 1983, Water Resour.Res.,19(3),718-724.
#
#   as implemented in the OTIS modelling framework
#
#   Model output has been checked against OTIS output
#
##########################################################################

#==============================================================================
# River spiral model with sorption
#==============================================================================

UvasCreek <- function() {
  new("odeModel",
    main = function(t,state,pars) {
      with (as.list(c(pars, inputs$boxes)),{
       C    <- state[1        :Nb    ]       # concentration in main channel
       Cs   <- state[(Nb+1)   :(2*Nb)]       # concentration in storage
       Csed <- state[(2*Nb+1) :(3*Nb)]       # sorbate concentration in riverbed

# upstream concentration at current time
       Cup =  Cfun(t)
#      print(Q)
#      print(Nb)

# transport - only C is transported
       tranC <- tran.volume.1D(C=C, C.up=Cup, flow=Q, flow.lat=Qlat, C.lat=Clat,
                          Disp=D, V=Vol)

# exchange between the two phases
       exch <- Alpha * (Cs - C)

# sorption
       sorption <- Rsorp*(Csed-C)

# the rate of change
       dC    <- tranC$dC   + exch          + sorption
       dCs   <-            - exch * A/As   + SorpS*(CsB-Cs)
       dCsed <-                            - Rho*sorption

       return(list(c(dC=dC,dCs=dCs,dCsed=dCsed),sorption=sorption,
                Flux.up = tranC$F.up,
                sorptot=sum(sorption*Vol),Sorbate=sum(Csed*Vol/Rho)))
      })
    },          
    # the model parameters:  
    parms = c(Q = 0.0125,              # m3/sec
              Clat = 0.13,             # Concentration lateral
              Sorp = 5.6e-5,           # sorption rate, /sec
              SorpS = 1,               # /second
              CsB = 0.13
    ),
    times = seq(8.25*3600,24*3600,by = 120),

    # this model comes with a user defined solver,
    #   i.e. a function instead of a character that points to an existing solver

    solver = function(y, times, func, parms, ...) {
      # steady-state condition of state variables, one vector
      ST <- steady.1D(y, time=0, func = func, nspec = 3, parms=parms, pos=TRUE)
      Dyn <- ode.1D(y=ST$y, func = func, nspec = 3, parms=parms, times=times)

      Nb <- length(ST$y)/3      # Nb known via inputs$boxes, but not here?
      # rearrange as data.frame
      times <- Dyn[,1]
      Dyn   <- Dyn[,-1]
        list(times=times,C=Dyn[,1:Nb], Cs=Dyn[,(Nb+1):(2*Nb)], Csed=Dyn[,(2*Nb+1):(3*Nb)])
    },

    # 1. a forcing function time series with tracer concentration
    # injected upstream
    inputs = list(timedep = data.frame(
       Time = c(8.25,8.399,8.400,11.399,11.4,   50) * 3600, # seconds
       C_up = c(0.13, 0.13,1.73,  1.73, 0.13, 0.13)
       )
    ),

    # derived and additional inputs which require internal calculations,
    # initfunc is called automatically during object creation
    initfunc = function(obj) {
     pars <- parms(obj)
      with(as.list(pars),{

        # box size, metres
        dx <- 1

# 5 segments, with different parameter values
        riverlen <- c(    38,    67,     176,      152,      236)
        disp     <- c(  0.12,  0.15,    0.24,     0.31,     0.40)
        area     <- c(  0.3 ,  0.42,    0.36,     0.41,     0.52)
        area2    <- c(  0.05,  0.05,    0.36,     0.41,     1.56)
        alpha    <- c(     0,     0,    3e-5,     1e-5,   4.5e-5)
        qlat     <- c(     0,     0,4.545e-6, 1.974e-6, 2.151e-6)

# Distances (x), and position of each box in river segment (ix)
        x  <- seq(from=dx/2, to = sum(riverlen), by = dx)
        Nb <- length (x)         # total number of boxes

        ix <- NULL
        for ( i in 1:length(riverlen)) ix <- c(ix, rep(i,riverlen[i]/dx))

# Values of parameters in each box
        Disp  <- disp[ix]        # m2/sec
        A     <- area[ix]        # m2
        As    <- area2[ix]       # m2
        Alpha <- alpha[ix]       # /sec
        Qlat  <- qlat[ix]        # m3/s, lateral flow rate
        D     <- Disp*A/dx       # m3/s, bulk dispersion coefficient
        Vol   <- A*dx            # m3  , river volume

        rho      <- c(0.35, 1.25, 0.8, 0.6, 0.35)
        rsorp    <- Sorp/rho

        Rho      <- rho[ix]         # mas/m3
        Rsorp    <- rsorp[ix]

        Cfun =approxfun(inputs(obj)$timedep, rule = 2)

        init(obj) = c(rep(0.13,2*Nb),rep(0,Nb))

        D[1] <-0
        inputs(obj)$boxes <- list(x=x, Disp = Disp, A = A, As = As, Alpha = Alpha,
          Qlat=Qlat, D=D, Vol=Vol, Cfun=Cfun, Nb=Nb, Rho=Rho, Rsorp=Rsorp)
        return(obj)
      })
    }
  )
}
