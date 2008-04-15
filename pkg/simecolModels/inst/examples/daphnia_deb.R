#################################################################################
# An example of the Dynamic energy budget model of Daphnia as presented in the  #
# publication of Rinke & Vijverberg (2005), Ecological Modelling 186, p326-344. #
# Further requests are welcome - please contact me!                             #
# Karsten Rinke, 4. August 2005 (karsten.rinke@uni-konstanz.de)                 #
#                                                                               #
# No starvation is taken into account - look at the EBT-version of the model    #
#                                                                               #
# simecol port by Thomas.Petzoldt@tu-dresden.de                                 #
#################################################################################


daphnia_deb <- function() {
  new("odeModel",
    main = function(time, init, parms){
        p <- as.list(parms)
        weight      <- init[1]
        egg         <- init[2]
        food        <- inputs$food
        t.kelvin    <- inputs$t.kelvin

        ## call of submodels
        ass.eff     <- ass.eff(food, p)
        ingestion   <- ingestion(food, t.kelvin, p) * weight^(2/3)
        respiration <- respiration(food, t.kelvin, p) * weight

        ## the differential equations:
        ## 1) somatic growth
        dweight <- p$kappa * ass.eff * ingestion - respiration

        ## 2) reproduction if the individual is mature
        if(weight2length(weight, p$l2w) > p$SAM) {
            degg <- (1 - p$kappa) * ass.eff * ingestion / p$cE
          } else {
            degg <- 0
        }
        list(c(dweight, degg))
    },
    ## Parametrization for Daphnia galeata
    parms = c(
      ## half saturation constant for ingestion (K_F; Muck1984)
      ing.ks          = 0.164,
      ## length to weight conversion (a; Urabe1991, Lynch1986)
      l2w             = 1.6,
      ## factor for food ingestion rate (p_ing; Urabe1991)
      ping            = 5.02,
      ## maximal metabolic rate at high food
      ## (m_F,max; Urabe1990, Lynch1986)
      resp.max        = 0.3,
      ## maximal metabolic rate at food close to zero
      ## (m_F,min; Urabe1990, Bohrer1988)
      resp.min        = 0.15,
      ## maximal asimilation efficiency at food close to zero
      ## (E_A,max; Urabe1991, Porter1982)
      ass.eff.max     = 0.9,
      ## minimal assimilation efficiency at high food
      ## (E_A,min; Urabe1991, Porter1982)
      ass.eff.min     = 0.5,
      ## energy allocation into somatic growth and metabolism
      ## (K; Kooijmann2000)
      kappa           = 0.35,
      ## carbon investment per egg (cE; Rinke2005)
      cE              = 1.3,
      ## Arrhenius temperature for Daphnia (T_A; Kooijmann2000)
      t.arrh          = 6400,
      ## Size at maturity - production of eggs starts (SAM; this study)
      SAM             = 1.2,
      ## size of neonates (SON; this study)
      SON             = 0.65
    ),
    ## Environmental conditions: temperature (K) & food (mgC/L S.acutus)
    inputs = data.frame(
      t.kelvin  = 20 + 273.15,  # 20°C
      food      = 1             # mgC/L
    ),
    times = c(from = 0, to = 50, by = 1),
    init = c(weight=0, egg=0),
    ## Equations are stored in an external list of functions which are
    ## shared between different models
    equations = daphnia_deb_equations,
    solver = "lsoda",
    initfunc = function(obj) {
       parms     <- as.list(parms(obj))
       length    <- 0.65
       weight    <- length2weight(length, parms$l2w)
       init(obj)  <- c(weight = weight, egg = 0)
       obj
    }
  )
}