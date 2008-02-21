#####################################################################
## Individual-based re-implementation in R of:
##
## Growth submodel after Rinke & Petzoldt 2003, Limnologica
##
##
## Version with additional state variable z (vertical location)
#####################################################################

daphnia_ibm_dvm <- function() {
 new("indbasedModel",
    main = function (time, init, parms) {
      #cat("time=", time, "\n")
      init <- live(init, parms)
      init <- move(time, init, parms)
      init <- survive(init, parms)
      init <- hatch(init, parms)
      init
    },
    equations = list(
      newdaphnia = function(SON, n, z=0) {
        if (n>0) {
          data.frame(age = rep(0, n), size = SON, eggs = 0, eggage = 0, z = z)
        } else {
          NULL
        }
      },
      bottrell = function(temp) {
        exp(3.3956 + 0.2193 * log(temp) - 0.3414 * log(temp)^2)
      },
      tefi = function(time, temp, food, parms){
        with(parms, {
          deltaL <- L_0 - L_0_Hall
          k      <- b1 * exp(b2 * temp)
          L_max  <- (a1 * food)/(a2 + food) + a3 - k * a4
          L      <- L_max - (L_max - L_0_Hall) * exp (-k * time) + deltaL
          E      <- (X_max_slope * food)/(K_s_slope + food) * L +
                      beta_min * (1 - exp(-u_c * food))
          as.data.frame(cbind(L, E))
      })},
      live = function(inds, parms){
        with(parms,{
          ninds       <- nrow(inds)
          inds$age    <- inds$age + DELTAT
          inds$eggage <- ifelse(inds$size > SAM & inds$eggs > 0,
                                inds$eggage + DELTAT, 0)
          tefi_out    <- tefi(inds$age, temp, food, parms)
          inds$size   <- tefi_out$L
          neweggs     <- round(tefi_out$E)
          inds$eggs   <- ifelse(inds$size > SAM & inds$eggage==0,
                                neweggs, inds$eggs)
          inds
      })},
    
      survive = function(inds, parms) {
        abundance <- nrow(inds)           # Zeilen = Individuen
        survive   <- runif(abundance)     # Zufallsgenerator
        mort      <- parms$mort * parms$DELTAT
        subset(inds, inds$age < parms$maxage & survive > mort)
      },
      hatch = function(inds, parms) {
        newinds <- NULL
        with(parms, {
          have.neo  <- 0
          new.neo   <- 0
          edt       <- bottrell(temp)
          have.neo  <- which(inds$eggs > 0 & inds$eggage > edt)
      
          eggs      <- inds$eggs[have.neo]
          z         <- inds$z[have.neo]
          z <- rep(z, eggs)
          new.neo   <- sum(eggs)
          inds$eggs[have.neo]   <- 0
          inds$eggage[have.neo] <- 0
          newinds <- newdaphnia(L_0, new.neo, z)
          rbind(inds, newinds)
        })
      },
      move = function(time, init, parms) {
        # dummy function, without movement
        init
      }
    ),
    parms = list(
      # parameters of the somatic growth equation
      a1          = 1.167,    # (mm)
      a2          = 0.573,    # (mg L^-1)
      a3          = 1.420,    # (mm)
      a4          = 2.397,    # (d),
      b1          = 1.089e-2, # (d^-1)
      b2          = 0.122,    # ((deg. C)^-1)
      # parameters of the clutch size equation
      X_max_slope = 23.83,    # (eggs)
      K_s_slope   = 0.65,     # (mg L^-1)
      beta_min    = -29.28,   # (eggs)
      u_c         = 1,        # (L mg^-1) unit conversion factor
      # parameters of the individual-based model
      L_0_Hall    = 0.35,     # (mm) SON (size of neonanates) of the Hall data
      L_0         = 0.65,     # (mm) SON
      SAM         = 1.50,     # (mm) SAM (size at maturity)
      maxage      = 60,       # (d)
      # constant environmental conditions
      temp        = 20,       # (deg C)
      food        = 0.5       # (mg L^-1)
    ),
    init = data.frame(age=0, size=0.65, eggs=0, eggage=0),
    times = c(from=0, to=60, by=1),
    solver = "iteration",
    initfunc = function(obj){
    SON <- parms(obj)$L_0
    SAM <- parms(obj)$SAM
    init(obj) <- data.frame(
      age    = c(rep(0, 20),rep(5, 30)),
      size   = c(rep(SON, 20),rep(SAM, 30)),
      eggs   = c(rep(0, 20),rep(5, 30)),
      eggage = c(rep(0, 20),rep(3, 30)),
      z      = runif(50, min=1, max=39)
    )
    obj
  }
  )
}

