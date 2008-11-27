################################################################################
#
# OmexDia - a model of Carbon, Nitrogen and oxygen diagenesis in (marine) sediments
# Soetaert et al., 1996.
#
# Model describing the dynamics of biogenic and dissolved silicate
# in a marine sediment. (Soetaert and Herman, 2008).
#
# the Newton-raphson method is used to solve for the steady-state condition
# Solved with rootSolve
#
################################################################################

OmexDiaDLL <- function() {
  new("odeModel",
    main = function(time = 0, state, parms) {
    },
    parms = c(

      # sediment parameters
      N        = 200,        # Total number of boxes, dummy value before initialising...
      sedDepth = 15,         # Total depth of modeled sediment (cm)
      thick    = 0.1,        # thickness of sediment layers (cm)
      por0     = 0.9,        # surface porosity (-)
      pordeep  = 0.7,        # deep porosity    (-)
      porcoef  = 2  ,        # porosity decay coefficient  (/cm)
      
      dB0      = 1/365,      # cm2/day       - bioturbation coefficient
      dBcoeff  = 2    ,
      mixdepth = 5    ,      # cm
      
      MeanFlux = 20000/12*100/365,  # nmol/cm2/d - Carbon deposition: 20gC/m2/yr
      rFast    = 0.01            ,  #/day        - decay rate fast decay detritus
      rSlow    = 0.00001         ,  #/day        - decay rate slow decay detritus
      pFast    = 0.9             ,  #-           - fraction fast detritus in flux
      w        = 0.1/1000/365    ,  # cm/d       - advection rate
      NCrFdet  = 0.16            ,  # molN/molC  - NC ratio fast decay detritus
      NCrSdet  = 0.13            ,  # molN/molC  - NC ratio slow decay detritus
      
      # Nutrient bottom water conditions
      bwO2     = 300             ,  #mmol/m3     Oxygen conc in bottom water
      bwNO3    = 10              ,  #mmol/m3
      bwNH3    = 1               ,  #mmol/m3
      bwODU    = 0               ,  #mmol/m3
      
      # Nutrient parameters
      NH3Ads     = 1.3        ,  #-           Adsorption coeff ammonium
      rnit       = 20.        ,  #/d          Max nitrification rate
      ksO2nitri  = 1.         ,  #umolO2/m3   half-sat O2 in nitrification
      rODUox     = 20.        ,  #/d          Max rate oxidation of ODU
      ksO2oduox  = 1.         ,  #mmolO2/m3   half-sat O2 in oxidation of ODU
      ksO2oxic   = 3.         ,  #mmolO2/m3   half-sat O2 in oxic mineralisation
      ksNO3denit = 30.        ,  #mmolNO3/m3  half-sat NO3 in denitrification
      kinO2denit = 1.         ,  #mmolO2/m3   half-sat O2 inhib denitrification
      kinNO3anox = 1.         ,  #mmolNO3/m3  half-sat NO3 inhib anoxic degr
      kinO2anox  = 1.         ,  #mmolO2/m3   half-sat O2 inhib anoxic min
      
      # Diffusion coefficients, temp = 10dgC
#      Temp       = 10         ,               # temperature
      DispO2     = 0.955    +10*0.0386 ,    #a+ temp*b
      DispNO3    = 0.844992 +10*0.0336 ,
      DispNH3    = 0.84672  +10*0.0336 ,
      DispODU    = 0.8424   +10*0.0242 ,
      dx         = rep(0,    200)        ,
      dxInt      = rep(0,    201)      ,
      Porosity   = rep(1,    200)        ,
      IntPor     = rep(1,    201)      ,
      Db         = rep(1,    201)
    ),
    times  = 0,                     # t=0 for steady-state calculation
    initfunc = function(obj) {      # initialisation
      pars <- parms(obj)
      with(as.list(pars),{
        if(length(init(obj))==0) {init(obj) = rep(1, 6 * N)}
        Intdepth <- seq(0, by=thick, len=N+1)  # depth at upper layer interfaces
        Nint     <- N+1             # number of interfaces
        Depth    <- 0.5*(Intdepth[-Nint] +Intdepth[-1]) # depth at middle of each layer

        Porosity = pordeep + (por0-pordeep)*exp(-Depth*porcoef)     # porosity profile, middle of layers
        IntPor   = pordeep + (por0-pordeep)*exp(-Intdepth*porcoef)  # porosity profile, upper interface

        Db       = pmin(dB0,dB0*exp(-(Intdepth-mixdepth)*dBcoeff))  # Bioturbation profile
        inputs(obj)$boxes <- list(Depth=Depth,Intdepth=Intdepth,
                                  Porosity=Porosity,IntPor=IntPor,Db=Db)
        parms(obj)[35:length(parms(obj))] <- c(rep(thick,N),rep(thick,N+1),Porosity,IntPor,Db)
        
        return(obj)
      })
    },
    # this model comes with a user defined solver,
    #   i.e. a function instead of a character that points to an existing solver
    solver = function(y, times, func, parms, ...) {
      with (as.list(parms),{
       # steady-state condition of state variables, one vector
      out <- steady.1D (y=y, fun="omexdiamod",parms=unlist(parms[-(1:9)]),
                   maxiter=100,dllname="simecolModels",outnames=c("O2flux",
                   "NO3flux","NH3flux","ODUflux","TotMin","OxicMin","Denitri",
                   "Nitri"),
                   method="stodes",nspec=6,pos=TRUE,initfunc="initomexdia",nout=8)
      # rearrange as a list with a data.frame and single values
        list(y=data.frame(FDET = out$y[1:N          ],
                   SDET = out$y[(N+1)  :(2*N)],
                   O2   = out$y[(2*N+1):(3*N)],
                   NO3  = out$y[(3*N+1):(4*N)],
                   NH3  = out$y[(4*N+1):(5*N)],
                   ODU  = out$y[(5*N+1):(6*N)]),
        O2flux=out$O2flux,NO3flux=out$NO3flux,NH3flux=out$NH3flux,
        ODUflux=out$ODUflux,TotMin=out$TotMin,OxicMin=out$OxicMin,
        Denitri=out$Denitri,Nitri=out$Nitri
                   )
      })
    }
  )
}
