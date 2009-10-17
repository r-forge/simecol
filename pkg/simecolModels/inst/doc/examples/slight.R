`slight` <-
function() {
  new("odeModel",
    main = function(time, init, parms, ...) {
      ## The state variables
      p<-init[1]  # phosphorus (ug P / l)
      x<-init[2]  # phytoplankton (mg C /l)
      z<-init[3]  # zooplankton (mg C /l)

      input <- as.list(approxTime1(inputs, time))

      temp <- input$temp
      i0   <- input$i0
      ice  <- input$ice
      mixz <- input$mixz
      pin  <- input$pin
      qin  <- input$qin
      vol  <- input$vol
      #print(time)

      with (as.list(parms),({
        ## ============= phytoplankton =========================================
        ## Phosphorus import
        pimport        <- qin/vol * (pin - p)

        if (xcomp == FALSE) {
          phoxp   <- p/(kp + p)                      # without competition
        } else {
          phoxp   <- p/(kp + p) * (1/x)/(1/kx + 1/x) # with competition
        }

        ## temperature dependence
        phoxt   <- photxmin + temp/toptx * (photxmax - photxmin)

        ## light dependence
        i0 <- i0 * (1-0.9 * ice)
        epges   <- epx * x + ep

        imean   <- (i0 - i0 * exp(-epges * mixz))/(epges * mixz)
        if (iz == FALSE) {
          ## imean-version, production dependend from mean light
          phoxl   <- imean/(ki + imean)
        } else {
          ## i-depth-version, analytically integrated over depth
          phoxl <- 1/(epges*mixz) *
                    log((i0 + ki)/(ki + i0 * exp(-epges * mixz)))
        }

        photosynthesis <- phoxl * phoxp * phoxt

        respiration    <- rxtmin + temp/toptx * (rxtopt - rxtmin) +
                                   rxmf * photosynthesis
        growth         <- (photosynthesis - respiration) * x
        sedimentation  <- x * vs/mixz

        ## ============= zooplankton ===========================================
        ## Arrhenius Eq. for zooplankton
        ## egg development time after Bottrell, egg=reduction factor
        dt <- 1/(dtb * exp(taegg / tref - taegg / (temp + 273.15)))
        if (dt < dtmin) egg <- 1 else egg <- dtmin/dt
        ## reduced grazing via egg development time reduction factor
        grazing   <- egg * z * gmax * x/(x + kxg)
        mortz     <- momin + mot * temp * z/(kmo + z)
        mortality <- z * mortz
        zooexkr   <- cp * (1 - ae + rz) * grazing + cp * mortality

        zgrowth   <- (ae - rz) * grazing

        ## ============ state equations ========================================
        dp <- pimport + zooexkr   - 1/cp * growth
        dx <- growth  - grazing  - sedimentation
        dz <- zin     + zgrowth  - mortality

        list(c(dp, dx, dz))
      }))
    },
    parms = c(
        zin     = 0,     # zooplankton-import (Carbon, mu g/L)
        kxg     = 0.25,  # half sat. ingest. zooplankton [SALMO/C] alternative: 0.164 [cit. RUDOLF]
        gmax    = 0.8,   # max. ingestion of zooplankton mean value between min (0.26) and max (1.3) [SALMO]
                         # temperature dependence ignored
        ae     = 0.6,    # assimilation efficiency [simplified from SALMO, AQUAMOD, Lars=0.7]
        ep      = 0.2,   # extinction of plankton free water (1/m) [lake specific]
        epx     = 0.425 ,# spec. extinktion of phytopl (m^2/mg C) Lampert & Sommer * 500/20, Rudolf =0.5
        ki      = 30,    # half sat. const light (J/(cm^2*d)) [SALMO, rounded]
        kp      = 1.7,   # half sat. const phosphorus (mg/m^3) [SALMO, Diat]
        kx      = 0.125, # strongly simplified from [SALMO/C] (value of kxmin)
        photxmax= 1.8,   # max photosynth. rate (1/d) [SALMO]
        photxmin= 0.17,  # min photosynth. rate (1/d) [SALMO]
        cp      = 0.04,  # C:P ratio mg C / ug P, [Redfield, SALMO Diat]
        toptx   = 20,    # optimal temp. for photosynthesis (°C) [SALMO Diat]
        vs      = 0.1,   # sedimentation velocity (m/d) [SALMO Diat]
        rxtopt = 0.06,   # resp rate 1/d at opt temp [SALMO, Diat]
        rxtmin = 0.02,   # T dependence of resp (1/°C) [SALMO, Diat]
        rxmf   = 0.3,    # light dpendend part of phytopl. respiration [SALMO]
        kmo =  0.0175,   # half sat. const. zoopl. mort. from zoopl. (mg C/l) [SALMO/C]
        momin = 0.015,   # Zooplankton mortality near 0°C (1/d) [SALMO]
        mot = 0.006,     # temperature dependence of zooplankton mortality (°C/d) [SALMO]
        rz = 0.2,        # respiration of zooplankton (20% of grazing), assuption
        dtb = 0.3720601, # inverse of egg development time at 20°C (d) [BOTTRELL]
        dtmin = 5,       # minimum egg development time (d) [SALMO]
        tref = 293.15,   # reference temperature 20°C (K)
        taegg = 11500,   # Arrhenius temperature egg development time (K)
                         # approximated after the curve of Bottrell [BOTTRELL, KOOIJMAN]
        iz = TRUE,       # i-depth version (TRUE) or i-mean version
        xcomp = TRUE     # phytopl. self limiting (TRUE) or not
    ),
    times = c(from=0, to=365, by=.5),
    init = c(phos=4, phyto=0.01, zoo=0.01),
    solver = "rk4",
    # inputs = data.frame(time=time, temp=temp, i0=i0, ice=ice,
    #                     mixz=mixz, pin=pin, qin=qin, vol=vol),
    initfunc = function(obj) {
      time <- times(obj)
      ## inputs with 1d timestep, independently from integration interval
      time <- min(time) : (max(time)+1)

      ## the following is an example data set, 
      ## replace inputs with your own measured data
      ## these lines are only executed if "inputs" is still empty
      if (is.null(inputs(obj))) {
        print("initializing input data")
        inputs(obj) <- as.matrix(data.frame(
          time = time,
          ## (deg C) replace with own fkt.!!!
          temp    = 12+10*sin((time+220)*pi/182),
          ## PAR in Dresden (fitted after DWD data, J/cm^2/d)
          i0     = 0.5 * (997 - 816 * cos(2*pi*time/365)
                      + 126 * sin(2*pi*time/365)),
          ## (ice cover)
          ice     = ifelse(time < 60 | 330 < time, 1, 0),
          ## mixing depth (m)
          mixz    = 10,
          ## mg/m3
          pin     = 20,
          ## water inflow (m3/d)
          qin     = 100000,
          # Lake volume (m^3)
          vol     = 35e6
        ))
      }
      obj
    }
  )
} # end function slight

