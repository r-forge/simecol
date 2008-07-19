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

OmexDia <- function() {
  new("odeModel",
    main = function(time = 0, state, parms) {
      with (c(as.list(parms),inputs$boxes), {

Flux  <- MeanFlux * (1+sin(2*pi*time/365))

FDET  <- state[1:N]
SDET  <- state[(N+1)  :(2*N)]
O2    <- state[(2*N+1):(3*N)]
NO3   <- state[(3*N+1):(4*N)]
NH3   <- state[(4*N+1):(5*N)]
ODU   <- state[(5*N+1):(6*N)]

# Transport fluxes
#==================

# Solid substances: upper boundary = deposition, lower=zero-gradient
FDETFlux<- -Db*(1-IntPor)*diff(c(FDET[1],FDET,FDET[N]))/thick   + #diffusion
            w * c(FDET[1],FDET)                                   #advection
FDETFlux[1] <-  Flux*pFast                                        #deposition

SDETFlux<- -Db*(1-IntPor)*diff(c(SDET[1],SDET,SDET[N]))/thick   +
            w * c(SDET[1],SDET)
SDETFlux[1] <-  Flux*(1.-pFast)

# Solute substances: upper boundary=imposed concentration
O2Flux   <- -DispO2  *   IntPor *diff(c(bwO2  ,O2 ,O2[N] ))/thick    +
            w * c(bwO2 ,O2)

NO3Flux  <- -DispNO3 *   IntPor *diff(c(bwNO3 ,NO3,NO3[N]))/thick    +
            w * c(bwNO3 ,NO3)

NH3Flux  <- -DispNH3 *   IntPor *diff(c(bwNH3 ,NH3,NH3[N]))/thick    +
            w * c(bwNH3 ,NH3)

ODUFlux  <- -DispODU *   IntPor *diff(c(bwODU ,ODU,ODU[N]))/thick    +
            w * c(bwODU ,ODU)


# production of DIC and DIN, expressed per cm3 LIQUID/day
DICprod_Min <- (rFast*FDET         +rSlow*SDET       )*(1.-Porosity)/Porosity
DINprod_Min <- (rFast*FDET*NCrFdet+rSlow*SDET*NCrSdet)*(1.-Porosity)/Porosity

# oxic mineralisation, denitrification, anoxic mineralisation
Oxicminlim <- O2/(O2+ksO2oxic)                     # limitation terms
Denitrilim <- (1-O2/(O2+kinO2denit))*NO3/(NO3+ksNO3denit)
Anoxiclim  <- (1-O2/(O2+kinO2anox))*(1-NO3/(NO3+kinNO3anox))
Rescale    <- 1/(Oxicminlim+Denitrilim+Anoxiclim)

OxicMin    <- DICprod_Min*Oxicminlim*Rescale        # oxic mineralisation
Denitrific <- DICprod_Min*Denitrilim*Rescale        # Denitrification
AnoxicMin  <- DICprod_Min*Anoxiclim *Rescale        # anoxic mineralisation

# reoxidation and ODU deposition
Nitri      <- rnit  *NH3*O2/(O2+ksO2nitri)
OduOx      <- rODUox*ODU*O2/(O2+ksO2oduox)

pDepo      <- min(1,0.233*(w*365)^0.336 )
OduDepo    <- AnoxicMin*pDepo

# the rate of change=- Flux gradient    + biogeochemistry
dFDET <- -diff(FDETFlux)/thick/(1-Porosity) - rFast*FDET
dSDET <- -diff(SDETFlux)/thick/(1-Porosity) - rSlow*SDET
dO2   <- -diff(O2Flux) /thick/Porosity -  OxicMin      -2* Nitri -      OduOx
dNH3  <- -diff(NH3Flux)/thick/Porosity + (DINprod_Min  - Nitri) / (1.+NH3Ads)
dNO3  <- -diff(NO3Flux)/thick/Porosity - 0.8*Denitrific + Nitri
dODU  <- -diff(ODUFlux)/thick/Porosity + AnoxicMin  - OduOx - OduDepo

#

return(list(c(dFDET,dSDET,dO2,dNO3,dNH3,dODU),
            Cflux = Flux, O2flux=O2Flux[1],NH3flux=NH3Flux[1],
            NO3flux=NO3Flux[1],ODUflux=ODUFlux[1]))

       })
    },
    parms = c(
     # sediment parameters
# N should NOT be a parameter; it shoudl be calculated, but i cannot make the
# model work unless N is a parameter...
     N        = 300,        # Total number of boxes
     thick    = 0.05,       # thickness of sediment layers (cm)
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
     Temp       = 10         ,               # temperature
     DispO2     = 0.955    +10*0.0386 ,    #a+ temp*b
     DispNO3    = 0.844992 +10*0.0336 ,
     DispNH3    = 0.84672  +10*0.0336 ,
     DispODU    = 0.8424   +10*0.0242

    ),
    times  = 0,                     # t=0 for steady-state calculation
    initfunc = function(obj) {      # initialisation
      pars <- parms(obj)
      with(as.list(pars),{
# THIS DID NOT WORK
#        Intdepth = seq(0,10,by=thick)         # depth at upper interface of each layer
        Nint     = N+1                             # number of interfaces
        Intdepth = seq(0,by=thick,len=Nint)        # depth at upper interface of each layer

        Depth    = 0.5*(Intdepth[-Nint] +Intdepth[-1]) # depth at middle of each layer
# THOMAS: N should be estimated here, but then I cannot access N in the solver
# so I had to make N a parameter can this be fixed?....
#       N        = length(Depth)                       # number of layers

        init(obj) = rep(1, 6 * N)                      # initial condition; any positive number will do
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
      out <- steady.1D(y, time=times, func, parms, nspec=6, pos=TRUE, ...)$y
      # rearrange as data.frame
      with (as.list(parms),
        data.frame(FDET = out[1:N          ],
                   SDET = out[(N+1)  :(2*N)],
                   O2   = out[(2*N+1):(3*N)],
                   NO3  = out[(3*N+1):(4*N)],
                   NH3  = out[(4*N+1):(5*N)],
                   ODU  = out[(5*N+1):(6*N)])
      )
    }
  )
}
