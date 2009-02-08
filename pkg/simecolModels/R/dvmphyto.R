################################################################################
#  P - X1/X2 - Z - Lake Model with Pulsed Grazing
# (Petzoldt et. al., 2009, Ecological Modelling)
#
# The model was implemented in the R programming language for statistical
# computing using the object oriented "simecol" package
# (Petzoldt & Rinke, 2007, Journal of Statistical Software 22(9))
#
################################################################################

dvm_phyto <- function() {

  new("odeModel",
    main = function(time, init, parms) {
      X <- init[1:2]
      Z <- init[3]
      P <- init[4]
      with(parms, {
        t       <- (time %% 24)
        .I_0    <- I_0(t, I_phot, I_ref, I_max, a, b, d)
        .ing_i  <- ing_i(X, ing_max_i, k_ing)
        .phot_i <- phot_i(P, X, phot_max_i, epsilon_min, epsilon_X,
                     .I_0, k_I, k_P, z_mix)
        .resx_i <- resx_i(phot_max_i, I_komp, k_I)  * 0.6
        .mort   <- mort(Z, mort_max, k_mort)
        .remin  <- remin(X, Z, yield_fae, ae, .ing_i , .mort)
        .resz   <- resz(resz_max, .ing_i, ing_max_i)

        if (DVM & ((time - 5) %% 24 + 1 <= 16)) {
          .ing_i <- .resz <- .mort <- .remin <- 0
        }

        ## Zooplankton
        dZ <-  (ae * sum(.ing_i) - .resz - .mort) * Z

        ## Phytoplankton (2 species, uses vectorized notation of R)
        dX_i <- (.phot_i - sed_i(v_sed, z_mix) - .resx_i)  *  X -
                  .ing_i *  Z + imp_X_i

        ## Phosphorus
        dP <- yield_CP * (.remin - (sum((.phot_i - .resx_i) * X))) + imp_P

        ret <- c(dX_i[1], dX_i[2], dZ, dP)
        names(ret) <- names(init)

        list(ret)
      })
    },
    equations = list(
      remin = function(X, Z, yield_fae, ae, .ing_i, .mort) {
        yield_fae * (.mort + (1 - ae) * sum(.ing_i)) * Z
      },
      ing_i = function(X, ing_max_i, k_ing) {
        ing_max_i  *  X / (sum(X) + k_ing)
      },
      resz = function(resz_max, .ing_i, ing_max_i) {
        resz_max * (0.5 + 0.5 * sum(.ing_i)/max(ing_max_i))
      },
      resx_i = function(phot_max_i, I_komp, k_I) {
        phot_max_i  * I_komp/(k_I + I_komp)
      },
      mort = function(Z, mort_max, k_mort) {
        mort_max  *  Z/(Z + k_mort)
      },
      phot_i = function(P, X, phot_max_i, epsilon_min,
        epsilon_X, .I_0, k_I, k_P, z_mix) {
        .epsilon <- epsilon(X, epsilon_min, epsilon_X)
        phot_max_i * P / (P + k_P)  * 1/.epsilon  *
          log ((.I_0 + k_I)/(k_I + .I_0  *  exp(-.epsilon *  z_mix))) /z_mix
      },
      epsilon = function(X, epsilon_min, epsilon_X) {
        epsilon_min + sum(epsilon_X  *  X)
      },
      I_0 = function(t, I_phot, I_ref, I_max, a, b, d) {
        I_phot  *  (1-I_ref)  *  I_max * 0.5 * (1 + sin(pi / b *  (t - a)) +
          sqrt(d + (sin(pi/b*(t - a)))^2) - sqrt(d + 1))
      },
      sed_i = function(v_sed, z_mix) {
        v_sed/z_mix
      }
    ),
    times  = c(from=0, to=100 * 24, by=1), # time step is hours
    init = c(Xr = 0.05, Xk = 0.05, Z = 0.05, P = 30), # in mg/L, P in \mug/L
    solver = "lsoda",
    parms = list(
      DVM           = FALSE,
      a             = 6,                 # Time of sunrise (6 h)
      ae            = 0.7,               # Assimilation efficency (0.7, nondimensional)
      b             = 16,                # Length of day (16 h)  -
      d             = 1e-3,              # Gloaming constant	(10^-3, nondimensional)
      epsilon_min   = 0.6,               # Background turbidity (0.6 m^-1)
      epsilon_X     = 0.5,               # Light absorption by phytoplankton (0.5 m^-1 mg,C^-1 l)
      I_komp        = 5 / 8.64,          # Light threshold for algal growth (0.58 W m^-2)
      I_max         = 560,               # Maximum value of irradiation  (560  W m^-2)
      I_phot        = 0.5,               # Ratio of photosynthetic active radiation of light (0.5, nondimensional)
      I_ref         = 0.1,               # Reflexion of light at water surface (0.1, nondimensional)
      imp_P         = 0.137 / 24,        # P import (0.5 g m^-2 a^-1)
      mort_max      = 0.045/24,          # Maximal mortality of Z (0.045 d^-1)
      k_I           = 29/8.64,           # Half saturation coefficient light (3.36 W m^-2)
      k_ing         = 0.164,             # Half saturation coefficient ingestion (0.164  mg,C,l^-1)
      k_mort        = 0.0158,            # Half saturation coefficient mortality of Z (0.016 mg,C,l^-1)
      k_P           = 5,                 # Half saturation coefficient phosphorus (5 mu ,g,P,l^-1)
      resz_max      = 1/24 * 0.288 * (2.66 * 1.5^3.09)^-0.15,  # Maximum respiration for Z (0.21 d^-1)
      temp          = 20,                # Temperature (20 degrees C)
      v_sed         = 0.1/24,            # Sinking velocity of phytoplankton (0.1 m d^-1)
      yield_CP      = 24.4,              # ratio of carbon to phosphorus (24.4 mgC (mu gP)^-1)
      yield_fae     = 0.7,               # ratio of resolved phosphorus from feaces (0.7, nondimensional)
      z_mix         = 10,                # Depth of epilimnion (10  m)
      phot_max_i    = c(2.5, 1) /24,     # Maximum growth rate for X_i (1.5, 0,6 d^-1)
      imp_X_i       = rep(0.0005, 2)/24, # Import of algal biomass (5e-04, 5e-04  mg,C,l^-1)
      ing_max_i     = rep(0.392/(2.66 * 1.5^3.09), 2) / c(1, 5)  # Maximal ingestion of phytoplankton group i  (1.0, 0.2  d^-1)
    )
  )
}
