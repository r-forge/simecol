#@ARTICLE{Rinke2003,
#  author = {Rinke, Karsten and Petzoldt, Thomas},
#  title = {Modelling the Effects of Temperature and Food on Individual Growth
#	   and Reproduction of \textit{Daphnia} and their Consequences on the
#	   Population Level},
#  journal = {Limnologica},
#  year = {2003},
#  volume = {33},
#  pages = {293--304},
#  number = {4}
#}



daphnia_tefi <- function() {
  new("timedepModel",
    main = function(time, init, parms, ...){
      #L_0  <- init[1]
      E    <- init[2]
      F    <- inputs$F
      T    <- inputs$T
      t    <- time
      with(parms, {
        deltaL <- L_0 - L_0_Hall
        k      <- b1 * exp(b2 * T)
        L_max  <- (a1 * F)/(a2 + F) + a3 - k * a4
        L      <- L_max - (L_max - L_0_Hall) * exp (-k * t) + deltaL
        E      <- (X_max_slope * F)/(K_s_slope + F) * L +
                    beta_min * (1 - exp(-u_c * F))
        list(c(L, E))
      })
    },
    ## Parametrization for Daphnia galeata
    parms = list(
      a1          = 1.167,
      a2          = 0.573,
      a3          = 1.420,
      a4          = 2.397,
      b1          = 1.089e-2,
      b2          = 0.122,
      X_max_slope = 23.83,
      K_s_slope   = 0.65,
      beta_min    = -29.28,
      u_c         = 1,
      L_0_Hall    = 0.35,
      L_0         = 0.65
    ),
    #Environmental conditions
    inputs = data.frame(
      T   = 20,  # temperature, 20°C
      F   =  0.2 # food (mgC/L Scenedesmus acutus)
    ),
    times = c(from = 0, to = 50, by = 1),
    solver = "iteration",
    #init = c(L=0.65, E=0),
    initfunc = function(obj) {
       parms     <- parms(obj)
       init(obj) <- c(L = parms$L_0, E = 0)
       obj
    }
  )
}


