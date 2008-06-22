################################################################################
#
# NPZriver - a model of succession of nutrients, phytoplankton and zooplankton
# in a river
# Soetaert and Herman, a practical guide to ecological modelling. Springer.
# chapter 7.9
#
# A model to investigate the response of nutrients, phytoplankton and
# zooplankton in a river, as a function of river flow.
#
# The biological component describes:
#   Nutrients, Phytoplankton and Zooplankton (NPZ model). 
# The physical component consists of advective transport term only 
# The river has constant cross-sectional surface, and is 100 km long. 
#
# the Newton-raphson method is used to solve for the steady-state condition
# Solved with rootSolve
#
################################################################################

NPZriver <- function() {
  new("odeModel",
    main = function(time = 0, state, parms) {
      with (as.list(parms), {
        N <- state[1       :Nb    ]      # nutrients
        P <- state[(Nb+1)  :(2*Nb)]      # phytoplankton
        Z <- state[(2*Nb+1):(3*Nb)]      # zooplankton
     
        delx     = riverlen / Nb
    
        # transport          #
        
        # advective fluxes at upper interface of each layer
        # freshwater concentration imposed 
  
        NFlux <- flow * c(RiverN, N) 
        PFlux <- flow * c(RiverP, P) 
        ZFlux <- flow * c(RiverZ, Z) 
   
        # Biology            #
  
        Pprod <-  mumax * N/(N + kN) * P  # primary production
        Graz  <-  gmax  * P/(P + kP) * Z  # zooplankton grazing
        Zmort <-  mrt * Z                 # zooplankton mortality
        
        # Rate of change = Flux gradient and biology
        dN    <- -diff(NFlux)/delx - Pprod + Graz*(1-eff) + Zmort
        dP    <- -diff(PFlux)/delx + Pprod - Graz 
        dZ    <- -diff(ZFlux)/delx         + Graz*eff     - Zmort 
  
        return(list(c(dN=dN, dP=dP, dZ=dZ), # rate of changes
                  Pprod=Pprod,              # Profile of primary production 
                  Graz=Graz,                # Profile of zooplankton grazing 
                  Zmort=Zmort,              # Profile of zooplankton mortality 
                  Nefflux=NFlux[Nb],        # DIN efflux
                  Pefflux=PFlux[Nb],        # Phytoplankton efflux
                  Zefflux=ZFlux[Nb]))       # Zooplankton efflux
       })
    },
    parms = c(
      riverlen = 100 ,              # total length river,         km
      Nb       = 100 ,              # number boxes
  
      RiverN   = 100 ,              # N concentration at river,   mmolN/m3
      RiverP   = 10  ,              # P concentration at river,   mmolN/m3
      RiverZ   = 1   ,              # Z concentration at river,   mmolN/m3
  
      flow     = 1   ,              # river flow,                 km/day
           
      mumax    = 0.5 ,              # maximal light-limited primary prod, /day
      kN       = 1   ,              # half-saturated N for pprod, mmolN/m3
      gmax     = 0.5 ,              # max grazing rate,           /day
      kP       = 1   ,              # half-saturated P for graz , mmolN/m3
      mrt      = 0.05,              # mortality rate,             /day
      eff      = 0.7 ,              # growth efficiency,          -
      flow     = 1                  # river flow,                 km/day
    ),
    times  = 0,                     # t=0 for steady-state calculation
    initfunc = function(obj) {      # initialisation
      pars <- parms(obj)
      with(as.list(pars),{
        init(obj) = rep(1, 3 * Nb)
        delx     = riverlen / Nb    # box length
        return(obj)        
      })
    },
    # this model comes with a user defined solver,
    #   i.e. a function instead of a character that points to an existing solver
    solver = function(y, times, func, parms, ...) {
      # steady-state condition of state variables, one vector
      out <- steady.1D(y, time=times, func, parms, nspec=3, pos=TRUE, ...)$y
      # rearrange as data.frame
      with (as.list(parms),
        data.frame(N=out[1:Nb], P=out[(Nb+1):(2*Nb)], Z=out[(2*Nb+1):(3*Nb)])
      )
    }
  )
}
