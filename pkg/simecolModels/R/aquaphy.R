################################################################################
#
# Aquaphy - a physiological model of unbalanced algal growth
# Lancelot et al., 1991.
# Soetaert and Herman, a practical guide to ecological modelling. Springer.
# chapter 2.8.2
#
# Phytoplankton model with uncoupled carbon and nitrogen assimilation.
# Algal biomass is described via 3 different state variables:
# - low molecular weight carbohydrates (LMW) that are the product of photosynthesis,
# - storage molecules (RESERVE) and
# - the biosynthetic and photosynthetic apparatus (PROTEINS).
#
# All state variables are expressed in mmol C m-3.
# Only proteins contain nitrogen and chlorophyll, at a fixed amount (i.e. using a fixed stoichiometric ratio).
# As the relative amount of proteins changes in the algae, so does the N:C and the Chl:C ratio.
# An additional state variable, dissolved inorganic nitrogen (DIN) has units of mmol N m-3.
#
################################################################################

Aquaphy <-  function() {
 new("odeModel",
   main = function(time, init, parms){
    with(as.list(c(init,parms)),{  # unpack the state variables, parameters

    # PAR, on-off function depending on the hour within a day
    hourofday       <- time%%24
    PAR <- ifelse (hourofday  < dayLength, parMean , 0)

    ## the output variables
    PhytoC           <- PROTEIN + RESERVE + LMW       # all components contain carbon
    PhytoN           <- PROTEIN * rNCProtein          # only proteins contain nitrogen
    NCratio          <- PhytoN / PhytoC
    Chlorophyll      <- PhytoN * rChlN
    TotalN           <- PhytoN + DIN
    ChlCratio        <- Chlorophyll / PhytoC

    ## the rates, in mmol/hr
    PartLMW          <- LMW / PhytoC
    Limfac           <- max(0,min(1,(maxpLMW -PartLMW)/(maxpLMW-minpLMW)))
    PhotoSynthesis   <- maxPhotoSynt*Limfac*(1-exp(alpha*PAR/maxPhotoSynt)) * PROTEIN
    Exudation        <- pExudation * PhotoSynthesis
    MonodQuotum      <- max(0,LMW / PROTEIN - minQuotum)
    ProteinSynthesis <- maxProteinSynt*MonodQuotum * DIN / (DIN+ksDIN)      * PROTEIN
    Storage          <- maxStorage    *MonodQuotum                          * PROTEIN
    Respiration      <- respirationRate * LMW + pResp*ProteinSynthesis
    Catabolism       <- catabolismRate  * RESERVE

    ## the rates of change of state variables; includes dilution effects (last term)
    dLMW     <- ( PhotoSynthesis + Catabolism
                - Exudation - Storage  - Respiration - ProteinSynthesis
                - dilutionRate * LMW)

    dRESERVE <-  Storage - Catabolism          - dilutionRate * RESERVE

    dPROTEIN <-  ProteinSynthesis              - dilutionRate * PROTEIN

    dDIN     <- -ProteinSynthesis * rNCProtein - dilutionRate * (DIN - inputDIN)


    ## the output, as a list
    list(c(dDIN,dPROTEIN,dRESERVE,dLMW),              ## the rate of change of state variables
           c(PAR               = PAR,                 ## the ordinary variables
             TotalN            = TotalN,
             PhotoSynthesis    = PhotoSynthesis,
             NCratio           = NCratio,
             ChlCratio         = ChlCratio,
             Chlorophyll       = Chlorophyll))
    })
    },
    parms = c(maxPhotoSynt   =0.125,      #molC/molC/hr      Maximal protein C-specific rate of photsynthesis at 20 dg
              rMortPHY       =0.001,      #/hr               Mortality rate of Phytoplankton (lysis and zooplankton grazing)
              alpha          =-0.125/150, #µEinst/m2/s/hr    Light dependency factor
              pExudation     =0.0,        #-                 Part of photosynthesis that is exudated
              maxProteinSynt =0.136,      #molC/molC/hr      Maximal Biosynthetic C-specific N-uptake rate
              ksDIN          =1.0,        #mmolN/m3          Half-saturation ct of N uptake Phytoplankton
              minpLMW        =0.05,       #molC/molC         Minimum metabolite/totalC ratio in algae
              maxpLMW        =0.15,       #molC/molC         Maximum metabolite/totalC ratio in algae
              minQuotum      =0.075,      #molC/molC         Minimum metabolite/Protein ratio for synthesis
              maxStorage     =0.23,       #/h                Maximum storage rate for Phytoplankton
              respirationRate=0.0001,     #/h                Respiration rate of LMW
              pResp          =0.4,        #-                 Part of protein synthesis that is respired (cost of biosynthesis)
              catabolismRate =0.06,       #/h                Catabolism rate of Phytoplankton reserves
              dilutionRate   =0.01,       #/h                dilution rate in chemostat
              rNCProtein     =0.2,        #molN/molC         Nitrogen/carbon ratio of proteins
              inputDIN       =10.0,       #mmolN/m3          DIN in inflowing water
              rChlN          =1,          #gChl/molN         Chl to nitrogen ratio
              parMean        =250.,       #µmolPhot/m2/s     PAR during the light phase
              dayLength      =15.         #hours             Length of illuminated period
              ),
    times  = c(from=0, to=10 * 24, by=1), # time step is hours
    init = c(DIN     =6.,     #mmolN/m3
              PROTEIN =20.0,   #mmolC/m3
              RESERVE =5.0,    #mmolC/m3
              LMW     =1.0),    #mmolC/m3
    solver = "lsoda"

  )
}


