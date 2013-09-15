################################################################################
## The estuarine ZOOPLANKTON model                                            ##
##                                                                            ##
## Soetaert and Herman, 1994. One foot in the grave: zooplankton drift into   ##
## the Westerschelde estuary (The Netherlands).                               ##
## Marine Ecology Progress Series 105: 19-29.                                 ## 
##                                                                            ##
## R-implementation given in:                                                 ##   
## Soetaert and Herman (2008). A practical guide to ecological modelling      ##
## -using R as a simulation platform. Springer                                ##
## Chapter 6. Model solution - numerical methods                              ##
## case study 6.6.5                                                           ##
################################################################################

#-----------------------------------------------------------#
# estuarine advective-dispersive transport and growth/decay #
#-----------------------------------------------------------#

ZooScheldt <- function() {
  new("odeModel",
    main = function(t, Zoo, pars, ...) {
      with (as.list(pars),{
        Flow   <- meanFlow + ampFlow * sin(2*pi*t/365 + phaseFlow)
        seaZoo <- approx(inputs$timedep$fZooTime, inputs$timedep$fZooConc, xout=t)$y
        Input  <- + Flow * c(riverZoo, Zoo) +
                  -inputs$boxes$Estar * diff(c(riverZoo, Zoo, seaZoo))
        dZoo   <- -diff(Input)/inputs$boxes$Volume + g * Zoo
        list(dZoo)
      })
    },          
    # the model parameters:  
    parms = c(nbox      = 100,          # number of boxes                           
              Length    = 100000,       # m       total estuarine length
              riverZoo  = 0.0,          # gDWT/m3 river marine zooplankton conc
              g         =-0.05,         # /day    growth rate
              meanFlow  = 100*3600*24,  # m3/d    mean river flow
              ampFlow   = 50*3600*24,   # m3/d    amplitude
              phaseFlow = 1.4       ,   # -       phase of river flow
              Eriver    = 0         ,   # m2/d    tidal dispersion at upstream boundary
              Esea      = 350*3600*24   # m2/d    tidal dispersion at downstream boundary
    ),
    times = 1:365,
    solver = "ode.band",

    # 1. a forcing function time series with zooplankton
    # concentration in the sea
    # Time and measured value of zooplankton concentration at sea boundary
    inputs = list(timedep = data.frame(
      fZooTime = c(0, 30,60,90,120,150,180,210,240,270,300,340,367),
      fZooConc = c(20,25,30,70,150,110, 30, 60, 50, 30, 10, 20, 20)
      )
    ),
    # derived and additional inputs which require internal calculations,
    # initfunc is called automatically during object creation
    initfunc = function(obj) {
     pars <- parms(obj)
      with(as.list(pars),{
        init(obj) = rep(0, nbox)

        # 2. estuarine morphology
        # cross sectional surface area is a sigmoid function of estuarine distance
        dx      <- Length/nbox                      # m
        IntDist <- seq(0,by=dx,length.out=nbox+1)   # m
        Dist    <- seq(dx/2,by=dx,length.out=nbox)  # m
        IntArea <- 4000 + 76000 * IntDist^5 /(IntDist^5+50000^5)   # m2
        Area    <- 4000 + 76000 * Dist^5    /(Dist^5+50000^5)      # m2
        Volume  <- Area*dx                          # m3

        # 3. Transport coefficients:
        # dispersion coefficients = a linear function of estuarine distance
        E        <- Eriver + IntDist/Length * Esea  # m2/d
        # bulk dispersion coefficient
        Estar  <- E * IntArea/dx                   # m3/d
        inputs(obj)$boxes <- list(Estar = Estar, Volume = Volume, Dist=Dist)
        return(obj)
      })
    }
  )
}
