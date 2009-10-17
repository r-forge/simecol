daphnia_deb_phyto <- function() {
  new("odeModel",
    parms = list(
      nphyto  = 3,
      K1      = 0.5,                   #Carrying Capacity mgC/L
      mu      = c(0.6, 0.7, 1.2),      #growth rate of phytoplankton 1/d
      t.arrhx = c(6936.9,              #Arrhenius temperature for X
                  9920.8,                
                  7744.4),
      pf      = c(0.1, 0.3, 1),        #preference factors for phytoplankton
  
      ## Parameters for Zooplankton
      ing.ks          = 0.164,         #half saturation constant for ingestion (K_F; Muck1984)
      l2w             = 1.6,           #length to weight conversion (a; Urabe1991, Lynch1986)
      ping            = 5.02,          #factor for food ingestion rate (p_ing;Urabe1991)
      resp.max        = 0.3,           #maximal metabolic rate at high food (m_F,max; Urabe1990, Lynch1986)
      resp.min        = 0.15,          #maximal metabolic rate at food close to zero(m_F,min; Urabe1990,Bohrer1988)
      ass.eff.max     = 0.9,           #maximal asimilation efficiency at food close to zero (E_A,max; Urabe1991,Porter1982)
      ass.eff.min     = 0.5,           #minimal assimilation efficiency at high food (E_A,min; Urabe1991,Porter1982)
      kappa           = 0.35,          #energy allocation into somatic growth and metabilism (K; Kooijmann2000)
      cE              = 1.3,           #carbon investment per egg (cE; Rinke2005)
      t.arrh          = 6400,          #Arrhenius temperature for Daphnia (T_A; Kooijmann2000)
      SAM             = 1.2,           #Size at maturity - production of eggs starts (SAM; this study)
      SON             = 0.65,          #size of neonates (SON; this study)
      bottrell.a      = 3.3956,        #parameter for calculation of egg development time according to Bottrell1976
      bottrell.b      = 0.2193,        #parameter for calculation of egg development time according to Bottrell1976
      bottrell.c      = -0.3414,       #parameter for calculation of egg development time according to Bottrell1976
      mort            = -0.05,         #background mortality rate in EBT (d_b; Huelsmann2002)
      starve.mort     = -0.35,         #mortality rate at strong starvation, i.e. W<W_crit (d_s; Rohrlack1999)
      critical.weight = 0.6,           #critical weight factor (p_m; Urabe1990,1991)
      life.span       = 25,            #maximal lifespan of an individual and the length of the EBTrain
      refVol = 1
    ),
    ## inputs may be variable in the future                                    
    inputs = data.frame(
      t.kelvin  = 17.5 + 273.15
    ),
    main = function(time, init, parms) {
        #extract variables from list
        statelist <- vec2list(init)
        p         <- as.list(parms)       # conversion is redundant
        food.dt   <- food <- statelist$X  # length of vector = 3
        zoo.dt    <- zoo  <- statelist$Z  # data frame with 5 cols
        t.kelvin  <- inputs$t.kelvin
       
        ###   ZOOPLANKTON
        ## call of submodels
        ass.eff     <- ass.eff(sum(food), p)
        ingestion <- matrix(0, nrow=nrow(zoo), ncol=p$nphyto)
        ii <- ingestion(food, t.kelvin, p)
        for (i in 1:p$nphyto) {
          ingestion[,i]  <- p$pf[i] *  ii[i] * zoo$weight^(2/3)
        }
        #ing.sum     <- ingestion1 + ingestion2 + ingestion3
        ing.sum <- rowSums(ingestion)
        respiration <- respiration(sum(food), t.kelvin, p) * zoo$weight
        egg.rel     <- 1/bottrell(t.kelvin, p)     
        #check for starvation
        opt.weight   <- length2weight(zoo$length, p$l2w)
        ind.kappa <- ifelse(zoo$weight < opt.weight, yes = 1, no = p$kappa) # starvation?
        ind.mort  <- ifelse(zoo$weight < p$critical.weight * opt.weight,    # strong starvation?
                                         yes = p$starve.mort + p$mort, no = p$mort)
  
        ## the differential equations:
        ## 0) age
        zoo.dt$age    <- 1
        ## 1) somatic growth
        zoo.dt$weight <- ind.kappa * ass.eff * ing.sum - respiration
        ## 2) reproduction 
        zoo.dt$egg <- 
          ifelse(zoo$weight > p$WAM, 
                 yes = (1 - ind.kappa) * ass.eff * ing.sum / p$cE,
                 no = 0
          )
        ## 3) length
        zoo.dt$length <- pmax(weight2length(zoo$weight, p$l2w) - zoo$length, 0)  
        ## 4) relative eggage. 1 = 100% developed egg
        relative.eggage <- egg.rel
        zoo.dt$eggage <- 
          ifelse(zoo$egg > 0 | zoo$eggstorage > 0,
                 yes = relative.eggage,
                 no  = 0
          )
        ## 5) eggstorage, eggnumber in brood chamber does not change
        zoo.dt$eggstorage <- 0
       
        ###   PHYTOPLANKTON
        wxx <- ingsum <- numeric(p$nphyto)
        sumfood <- sum(food)
        wxx <- wx(p$mu, p$K1, food, sumfood, t.kelvin, p$t.arrhx) 
        for (i in 1:p$nphyto) {
          #wxx[i] <- wx(p$mu[i], p$K1, food[i], sumfood, t.kelvin, p$t.arrhx[i]) 
          ingsum[i] <- sum(ingestion[,i])/1000/p$refVol
        }
        #differential equations for phytoplankton
        food.dt <-  wxx  - ingsum
        list(c(as.vector(food.dt), as.vector(unlist(zoo.dt))))
    },
    times = c(from = 0, to = 1, by = 0.1),
    equations = c(daphnia_deb_equations, daphnia_deb_lifeequations),
    solver="rk4",
    initfunc = function(obj) {
      p <- as.list(parms(obj))
      parms(obj)["WAM"] <- length2weight(p$SAM, p$l2w)  # weight at maturity
      parms(obj)["WON"] <- length2weight(p$SON, p$l2w)  # weight of neonates
      initlist <- list(X = c(x1=0, x2=0, x3=0.25),
                       Z = data.frame(
                         age        = c(1, 2, 3),
                         weight     = c(0.57, 0.90, 1.30),
                         egg        = 0,
                         eggage     = 0,
                         length     = c(0.71, 0.82, 0.93),
                         eggstorage = 0
                        )
      )
      init(obj) <- list2vec(initlist)
      obj
    }
  )
}