################################################################################
# Core Model of the P - X1/X2 - Z - Lake Model
# (Rudolf et. al., 2007, submitted)
#
# The model was implemented in the R programming language for statistical
# computing using the object oriented "simecol" package
# (Petzoldt & Rinke, 2007, Journal of Statistical Software 22(9))
#
################################################################################

dvm_phyto <- function() {
  new("odeModel",
    main = function(time, init, parms){
      Xr <- init[1]
      Xk <- init[2]
      Z  <- init[3]
      P  <- init[4]
  
      with(as.list(parms), {
        e <- 10 ^ -3  # duration of twilight
        # day night rhythm
        I.t <- I.max * 0.5 * (1 + sin(pi / 16 * (time %%24 - 6))
               + (e + sin(pi / 16 *(time %%24 - 6))^2 ) ^ 0.5 - (e + 1) ^ 0.5)
        #analytical Solution for combined Michalis-Menten and Lambert-Beer
        phot <- function(i0, ki, eps, z) {
                1/(eps) * log((i0 + ki)/(ki + i0 * exp(-eps*z)))}
        eps <- eps.min + Xr.eps * Xr + Xk.eps * Xk     #epsilon, see Scavia80
  
        Xr.hi <- phot(I.t, Xr.ki, eps, z.mix) / z.mix
        Xk.hi <- phot(I.t, Xk.ki, eps, z.mix) / z.mix
        Xr.mu <- Xr.mu.max *  (P / (P + Xr.kp)) * Xr.hi
        Xk.mu <- Xk.mu.max *  (P / (P + Xk.kp)) * Xk.hi
        Xr.growth <- Xr.mu * Xr
        Xk.growth <- Xk.mu * Xk
        Xr.sed <- Xr.sed.v / z.mix * Xr
        Xk.sed <- Xk.sed.v / z.mix * Xk
  
        ZXr.graz <- ZXr.graz.max * Xr / (ZXr.graz.ks + Xr + Xk) * Z
        ZXk.graz <- ZXk.graz.max * Xk / (ZXk.graz.ks + Xk + Xr) * Z
  
        Z.mort <- (Z.mort.min + Z.mort.temp * temp) * Z / (Z.mort.kmo + Z) * Z
  
        if ((time - 5) %% 24 + 1 <= 16) a <- TRUE else a <- FALSE
  
        if (a){
          if (DVM) {
            ZXr.graz   <- 0
            ZXk.graz   <- 0
            Z.resp.max <- 0
            Z.mort     <- 0
        }}
  
        ZXr.assi     <- ZXr.graz * Z.ae
        ZXk.assi     <- ZXk.graz * Z.ae
        Z.assi       <- (ZXr.graz + ZXk.graz) * Z.ae
        Z.assi.max   <- ZXr.graz.max * Z.ae
  
        Z.resp.grund <- Z.resp.max * 0.5
        Z.resp.assi  <- Z.resp.max * 0.5 *  Z.assi / Z.assi.max
        Z.resp       <- (Z.resp.grund + Z.resp.assi) * Z
  
        fae <- (1-Z.ae)*(ZXr.graz + ZXk.graz)
  
        dXr <- Xr.growth - Xr.sed - Xr.resp * Xr - ZXr.graz + Xr.imp
        dXk <- Xk.growth - Xk.sed - Xk.resp * Xk - ZXk.graz + Xk.imp
        dZ  <- ZXr.graz * Z.ae + ZXk.graz * Z.ae - Z.resp - Z.mort
        dP  <- {P.imp -
                yield.CP * Xr.growth - yield.CP * Xk.growth +
                yield.CP * Xr.resp * Xr + yield.CP * Xk.resp * Xk +
                yield.mort * yield.fae * Z.mort + yield.CP * yield.fae * fae}
  
        TE       <- (ZXr.graz + ZXk.graz) * Z.ae / (Xr + Xk)
        X.growth <- Xr.growth + Xk.growth
        Z.growth <- (ZXr.graz + ZXk.graz) * Z.ae
        ## as.numeric() below is only intended to suppress automatic naming
        ## list(c(state variables), c(other variables))
        list(c(dXr, dXk, dZ, dP),
             c(X.growth  = as.numeric(X.growth),
               Z.growth  = as.numeric(Z.growth),
               TE        = as.numeric(TE), a = a,
               I.t       = as.numeric(I.t),
               ZXr.graz  = as.numeric(ZXr.graz/Z),
               ZXk.graz  = as.numeric(ZXk.graz/Z),
               Xr.mu     = as.numeric(Xr.mu),
               Xk.mu     = as.numeric(Xk.mu),
               Xr.growth = as.numeric(Xr.growth - Xr.sed - Xr.resp * Xr),
               Xk.growth = as.numeric(Xk.growth - Xk.sed - Xk.resp * Xk),
               fae       = as.numeric(fae)
        ))
      })
    },
    parms = c(
      DVM              = TRUE,
      temp             = 20,          #   C
      eps.min          = 0.6,         #   1/m  Lehman75 / SALMO
      z.mix            = 10,          #   zmix = -eps.min * ln (ikomp/imax)
      I.max.global.max = 8640,        #   J / cm d = 1000W/m  daily maximal value
      I.meanmax        = 0.56,        #   dimensionless --> I.max.global on
                                      #   optimum conditions* I.meanmax = I.maxglobal
      ## Zooplankton
      Z.l              = 1.5,         #   mm
      ZXr.graz.ks      = 0.164,       #   mgC/l   Author? i.e. Gurney90/Takashi00
      ZXk.graz.ks      = 0.164,       #   mgC/l   Author? i.e. Gurney90/Takashi00
      ZXr.graz.max.ind = 0.392,       #   gC/Ind h   see zooplankton.r
      ##  gC/Ind h   see zooplankton.r + Geller(LR036) + book
      ZXk.graz.max.ind = 0.392/5,
    
      Z.mort.min.d     = 0.005,       #   /d  SALMO S.10
      Z.mort.temp.d    = 0.002,       #   /d  SALMO S.10
      Z.mort.kmo       = 0.0158,      #   mgC/l from mgFM/l  SALMO p.10
      Z.ae             = 0.7,         #   Takashi00 and Lynch86
                                      #  (Assimilation / Ingestion
                                      #    = AE <- 0.909 * W ^ -0.118 )
      ## Phytoplankton
      Xr.mu.max.d      = 1.5/0.6,     #   1/d   max photosynthesis per day / factor
      Xk.mu.max.d      = 0.6/0.6,     #   1/d   max photosynthesis per day / factor
      Xr.kp            = 5,           #   gP/l   SRP
      Xk.kp            = 5,           #   gP/l   SRP
      Xr.ki            = 29,          #   J / cm d, SALMO
      Xk.ki            = 29,          #   J / cm d, SALMO
      Xr.ikomp         = 5,           #   J / cm    i.e. Benndorf80
      Xk.ikomp         = 5,           #   J / cm    i.e. Benndorf80
      Xr.sed.v.d       = 0.1,         #   m / d     SALMO
      Xk.sed.v.d       = 0.1,         #   m / d     SALMO
      Xr.eps           = 0.5,         #   1/m * l/mgC
      Xk.eps           = 0.5,         #   1/m * l/mgC
      Xr.imp.d         = 0.0005,      #   phytopl. import mgC/l d
      Xk.imp.d         = 0.0005,      #   phytopl. import mgC/l d
    
      ## Phosphorus
      P.imp.d          = 0.137,       #   mgP/m3 d  P-import, slightly eutrophic
      yield.CP         = 24.4,        #   mgC / mugP   Yield Carbon per P (Redfield)
      yield.mort       = 24.4,        #   mugP / mgC   remin of P, Daphnienmort., SALMO
      yield.fae        = 0.7,         #   part of remineralized P of Zoopl. faeces, SALMO
      ## scenario parameters to change intermediate variables
      f.Z.resp         = 1
    ),
    times  = c(from=0, to=100 * 24, by=1), # time step is hours
    init = c(Xr = 0.05, Xk = 0.05, Z = 0.05, P = 30), # in mg/L, P in \mug/L
    solver = "lsoda",
    ## optional slot for time dependend data, e.g. time-dependent temperature
    #inputs = as.matrix(
    #  data.frame(
    #    time = c(0,   5, 50, 100),
    #    temp = c(4,   4, 10,  20)
    #  )
    #),
    ## initfunc is called during initialize() for first-time calculations
    initfunc = function(obj) {
      p <- parms(obj)
      pnew <- with(as.list(p), {
        I.max.global     <- I.max.global.max * I.meanmax
        ##  10% reflection at water surface
        I.max.global.Z0  <- I.max.global * 0.9
        ##  J / cm d   photosynthetic active Radiation at z < 0m
        I.max            <- I.max.global.Z0 / 2
        Z.w              <- 2.66 * Z.l ^ 3.09    #  gC/Ind  Geller75
        ZXr.graz.max     <- ZXr.graz.max.ind / Z.w
        ZXk.graz.max     <- ZXk.graz.max.ind / Z.w
        Z.mort.min       <- Z.mort.min.d / 24
        Z.mort.temp      <- Z.mort.temp.d / 24
        Z.resp.ind.d     <- f.Z.resp * 0.288  * Z.w ^ 0.85 #  gC/Ind d   Author?
        Z.resp.max       <- Z.resp.ind.d / Z.w / 24    #  gC/h   Author?
        Xr.mu.max        <- Xr.mu.max.d / 24           #  1/h
        Xk.mu.max        <- Xk.mu.max.d / 24           #  1/h
        ## value of 0.6 is backcalculation to daily rate of Xr.mu.max
        Xr.resp          <- Xr.mu.max * Xr.ikomp / (Xr.ki + Xr.ikomp)*0.6
        Xk.resp          <- Xk.mu.max * Xk.ikomp / (Xk.ki + Xk.ikomp)*0.6
        Xr.sed.v         <- Xr.sed.v.d / 24             #  m / h
        Xk.sed.v         <- Xk.sed.v.d / 24             #  m / h
        Xr.imp           <- Xr.imp.d / 24               #  Import mgC/l h
        Xk.imp           <- Xk.imp.d / 24               #  Import mgC/l h
        P.imp            <- P.imp.d / 24                #  mgP/m3 h
  
        c(I.max.global = I.max.global,
          I.max.global.Z0 = I.max.global.Z0, I.max = I.max, Z.w = Z.w,
          ZXr.graz.max = ZXr.graz.max,
          ZXk.graz.max = ZXk.graz.max, Z.mort.min = Z.mort.min,
          Z.mort.temp = Z.mort.temp, Z.resp.ind.d = Z.resp.ind.d,
          Z.resp.max = Z.resp.max, Xr.mu.max = Xr.mu.max, Xk.mu.max = Xk.mu.max,
          Xr.resp = Xr.resp, Xk.resp = Xk.resp, Xr.sed.v = Xr.sed.v,
          Xk.sed.v = Xk.sed.v, Xr.imp = Xr.imp, Xk.imp = Xk.imp, P.imp = P.imp)
      })
      ## important! new parameters should overwrite old ones
      parms(obj) <-  c(p[!(names(p) %in% names(pnew))], pnew)

      obj
    }
  )
}  
    
    
    
   