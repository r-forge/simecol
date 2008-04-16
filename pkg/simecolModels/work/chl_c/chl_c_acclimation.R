## Phytoplankton growth model 
##   with variable Chlorophyll:Carbon Cell quota
##
## Baumert, H. On the Theory of Phytosynthesis and Growth in Phytoplankton. 
## Part I: Light Limitation and Constant Temperature. 
## Int. Rev. Ges. Hydrobiol., 1996, 81, 109-139
##
## Baumert, H. and Petzoldt, T. The Role of Temperature, Cellular Quota and 
## Nutrient Concentrations for  Photosynthesis, Growth and Light-Dark 
## Acclimatation in Phytoplankton. Limnologica (accepted).
##
## Note that the second paper contains a model with nutrients and temperature
## as additional control variables. An implementation will coming soon.

library("simecolModels")

chl_c_acclimation <-function() {
  
  equations <- list(
    radday = function(time) {
      time <- (time * 24)  %% 24
      ifelse(6 < time & time <=18, 1, 0)
    },
    # light: imean
    imean = function(i0, mixz, epsmin, epsx, x) {
        epges   <- epsx * x + epsmin
        imean   <- (i0 - i0 * exp(-epges * mixz))/(epges * mixz)
    },
    # respiration rate
    resp_baumert = function(photx, gamma, zeta, rxt, rxmf) {
      (photx * zeta + rxt) / (1 + zeta)
    },
    resp_salmo = function(photx, gamma, zeta, rxt, rxmf) {
      rxt + rxmf * photx
    },
    pp = function(phox0, imean, alpha, beta, gamma) {
      #alpha <- phox0 /(ki * gamma)
      phox0 * (1-exp(-alpha * gamma * imean / phox0)) *
                 exp(-beta  * alpha * gamma * imean / phox0)
    },
    d.gamma = function(phox0, imean, alpha, beta, gamma, gammamin, gammamax) {
      gamma * phox0 *(exp(-alpha * gamma * imean/phox0) -
                             (gamma - gammamin)/(gammamax - gammamin)) *
                             exp(-beta * alpha * gamma * imean/phox0)
    }
  ) 
  
  chlcpp <- new("odeModel",
    main = function(time, init, parms) {
      x     <- init[1]  # phytoplankton (mg C /l)
      gamma <- init[2]  # chlorophyll:C relation (1)
      with (as.list(parms),({
        i0     <- radday(time) * PAR
        #imean  <- imean(i0, mixz, epsmin, epsx, x)
        imean<- imean(i0, mixz, epsmin, epschl, x * gamma)
        phox0  <- photxmax
        photx  <- pp(phox0, imean, alpha, beta, gamma)
        resp   <- resp(photx, gamma, zeta, rxt, rxmf)
        d.gamma <- d.gamma(phox0, imean, alpha, beta, gamma, gammamin, gammamax)
  
        mu     <- photx - resp # SALMO: wx = phot - rx    (9.4)
        dx     <- (mu - sed) * x
        list(c(dx, d.gamma),
             # as.vector drops original names and avoids name mangling
             c(imean   = as.vector(imean),
               photx   = as.vector(photx),
               mu      = as.vector(mu),
               NPP     = as.vector(dx),
               d.gamma = as.vector(d.gamma),
               GPPspec = as.vector(photx * gamma)
               )
             )
      }))
    },
    parms = c(
        epsmin   = 0.2,      # extinction of plankton free water (1/m)
                             # [lake specific]
        epsx     = 0.425,    # spec. extinktion of phytopl (m2/mg C)
                             # Lampert & Sommer * 500/20, Rudolf =0.5
        epschl   = 0.017 * 1000,
        photxmax = 1.3,      # max photosynth. rate (1/d) == mu^0_m in Baumert 1996
        alpha    = 0.34,
        beta     = 0.52,     # Inhibition term of photosynthesis
                             # called "epsilon" in Baumert 1996
                             # often close to zero (Baumert, pers. comm.)
        gammamax = 0.042,    # Baumert 1996
        gammamin = 0.00056,  # Baumert 1996
        rxt      = 0.06,     # dark respiration,  Benndorf, 1979
        rxmf     = 0.3,      # turnover dependend resp. (from photx) Benndorf, 1979
        zeta     = 0.18,     # turnover dependend resp.  (from growth)
        mixz     = 1,        # mixing depth   (m)
        sed      = 0.0,      # sedimentation rate (1/d)
        PAR      = 500       # W/m2
        
    ),
    equations = equations,
    times = c(from=1, to=5, by=0.02),
    init = c(carbon=1, gamma=0.0383),
    solver = "lsoda"
  )
  
  equations(chlcpp)$resp <- equations(chlcpp)$resp_salmo
  
  equations(chlcpp)$radday <- function(time) {
    x<-(sin((time - 6/24)*2*pi))
    x * (x > 0)
  }
  
  chlcpp
}