################################################################################
#
# NPZD - model of nutrients, phytoplankton, zooplankton and detritus in 0-D
# Soetaert and Herman, a practical guide to ecological modelling. Springer.
# chapter 2.8.1.
#
# Simple ecosystem model
#
################################################################################

NPZD <-  function() {
 new("odeModel",
   main = function(time, state, parameters){
   
     with(as.list(c(state,parameters)),{
        # Light, a sine function; 50% of light is PAR
        PAR            <- 0.5*(540+440*sin(2*pi*time/365-1.4))
        din            <- max(0,DIN)      # to avoid errors when DIN becomes slightly negative..
        Nuptake        <- maxUptake * PAR/(PAR+ksPAR) * din/(din+ksDIN)*PHYTO
    
        Grazing        <- maxGrazing* PHYTO/(PHYTO+ksGrazing)*ZOO
        Faeces         <- pFaeces * Grazing
        Excretion      <- excretionRate * ZOO
        Mortality      <- mortalityRate * ZOO * ZOO
        Mineralisation <- mineralisationRate * DETRITUS
        Chlorophyll    <- chlNratio * PHYTO
        TotalN         <- PHYTO + ZOO + DETRITUS + DIN
    
        dPHYTO    <- Nuptake - Grazing
        dZOO      <- Grazing - Faeces - Excretion - Mortality
        dDETRITUS <- Mortality - Mineralisation + Faeces
        dDIN      <- Mineralisation + Excretion - Nuptake
    
        # the output, packed as a list
        list(c(dPHYTO,dZOO,dDETRITUS,dDIN),                          # the rate of change
            c(Chlorophyll = Chlorophyll, PAR=PAR, TotalN= TotalN))   # the ordinary output variables
      })
    },

    parms = c(maxUptake          =1.0,       # /day
              ksPAR              =140,       # µEinst/m2/s
              ksDIN              =0.5,       # mmolN/m3
              maxGrazing         =1.0,       # /day
              ksGrazing          =1.0,       # mmolN/m3
              pFaeces            =0.3,       # -
              excretionRate      =0.1,       # /day
              mortalityRate      =0.4,       # /(mmolN/m3)/day
              mineralisationRate =0.1,       # /day
              chlNratio          =1          # mgChl/mmolN
               ),
    times  = seq(from=0, to=365, by=1),      # time step is days
    init   =  c(PHYTO   =1,                  # state variable initial conditions, units mmolN/m3
                ZOO     =0.1,
                DETRITUS=5.0,
                DIN     =5.0),
    solver = "lsoda"

  )
}


