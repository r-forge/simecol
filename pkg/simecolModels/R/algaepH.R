################################################################################
#
# algaepH- modelling the effect of algal growth on pH
# Soetaert and Herman, a practical guide to ecological modelling. Springer.
# chapter 8.5.2.
#
# Simple phytoplankton model with coupled pH dynamics
#
################################################################################

algaepH <-  function() {
  new("odeModel",
    main = function(time, init, parms, Nitrate=TRUE){
    
      # Internal function to find the root of pH
      pHfunction <- function(pH, k1, k2, DIC, Alkalinity )
      {
        H    <- 10^(-pH)
        HCO3 <- H*k1  /(H*(k1+H) + k1*k2)*DIC
        CO3  <- k1*k2 /(H*(k1+H) + k1*k2)*DIC
        
        EstimatedAlk  <- (- H) *1.e6  + HCO3 + 2*CO3
        
        return(EstimatedAlk  - Alkalinity)
      }
      
      with(as.list(c(init,parms)),{  # unpack the state variables, parameters
      
        PAR    <- 0.
        if(time%%24 < dayLength) PAR <- parDay
        
        Growth <- maxGrowth*DIN/(DIN+ksDIN)*PAR/(PAR+ksPAR)*ALGAE -
                    respRate * ALGAE
        
        dDIN   <- -Growth                       # DIN is consumed
        dDIC   <- -Growth * CNratio             # DIC is consumed ~ CN ratio
        dALGAE <- Growth                        # algae increase by growth
        ifelse (Nitrate, dALKALINITY <- Growth, # alkalinity production if growing on nitrate
                dALKALINITY <- -Growth)         # alkalinity consumption if growing on ammonium
        
        # estimate the pH
        pH  <- uniroot(pHfunction,lower=0,upper=12,tol=1.e-20,
               k1=k1,k2=k2, DIC=DIC,Alkalinity=ALKALINITY)$root
        
        list(c(dDIN, dALGAE, dALKALINITY, dDIC), c(PAR=PAR, pH=pH)  )
      })
    },
    # dissociation constants are available via package "seacarb"
    #  require(seacarb)
    #  Salinity     <- 0
    #  Temperature  <- 20
    #  WDepth       <- 0
    #  k1           <- K1(Salinity,Temperature,WDepth)    # Carbonate k1
    #  k2           <- K2(Salinity,Temperature,WDepth)    # Carbonate k2

    parms = c(k1=9.6095e-7  ,   #mol/kg sol    carbonate dissociation ct 1
              k2=3.41048e-10,   #mol/kg sol    carbonate dissociation ct 2
              maxGrowth =0.125, #molN/molN/hr  Maximal growth rate
              ksPAR     =100,   #µEinst/m2/s   Half-saturation ct for light-limited growth
              ksDIN     =1.0,   #mmolN/m3      Half-saturation ct of N uptake Phytoplankton
              respRate  =0.001, #/h            Respiration rate
              CNratio   =6.5,   #molC/molN     carbon:Nitrogen ratio
              parDay    =250.,  #µEinst/m2/s   PAR during the light phase
              dayLength =12.    #hours         Length of illuminated period (in one day)

                ),
    times  = seq(from=0, to=10 * 24, by=1), # time step is hours
    init =  c(DIN        =30,     #mmolN/m3
              ALGAE      =0.1,    #mmolN/m3
              ALKALINITY =2200,   #mmol/m3
              DIC        =2100),  #mmolC/m3
    solver = "lsoda"
  )
}


