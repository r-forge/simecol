################################################################################
#
# SiDia - a model of silicate diagenesis in marine sediments
# Soetaert and Herman, a practical guide to ecological modelling. Springer.
# chapters 3.6.5. and 7.8.5
#
# Model describing the dynamics of biogenic and dissolved silicate
# in a marine sediment. (Soetaert and Herman, 2008).
#
# the Newton-raphson method is used to solve for the steady-state condition
# Solved with rootSolve
#
################################################################################

SiDia <- function() {
  new("odeModel",
    main = function(time = 0, state, parms) {
      with (c(as.list(parms),inputs$boxes), {
        BSi<- state[1:N]              # Biogenic silicate (solid)
        DSi<- state[(N+1):(2*N)]      # Dissolved silicate
        
        # diffusive fluxes at upper interface of each layer
        # upper concentration imposed (bwDSi), lower: zero gradient
        DSiFlux <- -SedDisp *   IntPor *diff(c(bwDSi ,DSi,DSi[N]))/thick
        BSiFlux <- -Db      *(1-IntPor)*diff(c(BSi[1],BSi,BSi[N]))/thick
        
        BSiFlux[1] <- BSidepo                         # upper boundary flux is imposed
        
        # BSi dissolution    #
        
        Dissolution <- rDissSi * BSi*(1.- DSi/EquilSi )^pow
        Dissolution <- pmax(0,Dissolution)
        
        # Rate of change= Flux gradient, corrected for porosity and dissolution
        dDSi        <- -diff(DSiFlux)/thick/Porosity      +           # transport
                       Dissolution * (1-Porosity)/Porosity           # biogeochemistry
        
        dBSi        <- -diff(BSiFlux)/thick/(1-Porosity)  - Dissolution
        
        return(list(c(dBSi = dBSi, dDSi = dDSi),
                   Dissolution = Dissolution,
                   DSiSurfFlux = DSiFlux[1], DSIDeepFlux = DSiFlux[N+1],
                   BSiDeepFlux = BSiFlux[N+1]))
       })
    },
    parms = c(
      # sediment parameters
# N should NOT be a parameter; it shoudl be calculated, but i cannot make the
# model work unless N is a parameter...
       N        = 0,          # Total number of boxes (dummy value before initialization)
       thick    = 0.05,       # thickness of sediment layers (cm)
       por0     = 0.9,        # surface porosity (-)
       pordeep  = 0.7,        # deep porosity    (-)
       porcoef  = 2  ,        # porosity decay coefficient  (/cm)
  
       dB0      = 1/365,      # cm2/day       - bioturbation coefficient
       dBcoeff  = 2    ,
       mixdepth = 5    ,      # cm
  
       SedDisp  = 0.4  ,      # molecular diffusion coefficient, cm2/d
  
       # biogeochemical parameters
       rDissSi  = 0.005,      # dissolution rate, /day
       EquilSi  = 800  ,      # equilibrium concentration
       pow      = 1    ,
       BSidepo  = 0.2*100,    # nmol/cm2/day
       bwDSi    = 150         # µmol/l
    ),
    times  = 0,                     # t=0 for steady-state calculation
    initfunc = function(obj) {      # initialisation
      pars <- parms(obj)
      with(as.list(pars),{
## THIS DID NOT WORK
##        Intdepth = seq(0,10,by=thick)         # depth at upper interface of each layer
#        Nint     = N+1                             # number of interfaces
#        Intdepth = seq(0,by=thick,len=Nint)        # depth at upper interface of each layer
#
#        Depth    = 0.5*(Intdepth[-Nint] +Intdepth[-1]) # depth at middle of each layer
## THOMAS: N should be estimated here, but then I cannot access N in the solver
## so I had to make N a parameter can this be fixed?....
##       N        = length(Depth)                       # number of layers

## ThPe: Is this what you want?   
##  but: 
##    - I don't understand why you tried to compute Intdepth two times !!!
##    - where does the "10" come from ?  Should be a parameter
        Intdepth <- seq(0, 10, by=thick)
        Nint     <- length(Intdepth)
        N        <- Nint - 1
        Depth    <- 0.5*(Intdepth[-Nint] +Intdepth[-1]) # depth at middle of each layer
        
        parms(obj)["N"] <- N  # this ist the important step! write the new N back to parms
        # you can also store N in the init-slot (the states), but then you have
        # to define a new class with init as list. It may then also necessary to define
        # the appropriate slot functions (i.e. out)  and to re-define the solver
## end of alternative formulation
        init(obj) = rep(1, 2 * N)                      # initial condition; any positive number will do
        Porosity = pordeep + (por0-pordeep)*exp(-Depth*porcoef)     # porosity profile, middle of layers
        IntPor   = pordeep + (por0-pordeep)*exp(-Intdepth*porcoef)  # porosity profile, upper interface

        Db       = pmin(dB0,dB0*exp(-(Intdepth-mixdepth)*dBcoeff))  # Bioturbation profile
        inputs(obj)$boxes <- list(Depth=Depth,Intdepth=Intdepth,
                                  Porosity=Porosity,IntPor=IntPor,Db=Db)
        return(obj)
      })
    },
    # this model comes with a user defined solver,
    #   i.e. a function instead of a character that points to an existing solver
    solver = function(y, times, func, parms, ...) {
      # steady-state condition of state variables, one vector
      out <- steady.1D(y, time=times, func, parms, nspec=2, pos=TRUE, ...)$y
      # rearrange as data.frame
      with (as.list(parms),
        data.frame(BSi=out[1:N], DSi=out[(N+1):(2*N)])
      )
    }
  )
}
