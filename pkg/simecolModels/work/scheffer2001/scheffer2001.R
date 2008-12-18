################################################################################
# Original Model from:
#   Scheffer, M.; Straile, D.; van Nes, E. H. & Hosper, H.
#   Climatic warming causes regime shifts in lake food webs
#   Limnol. Oceanogr., 2001, 6, 1780–1783
#
# R Re-Implementation by Thomas Petzoldt
#   - formal change: consistent capitalization of F and h_Z
################################################################################

scheffer2001 <-  function() {
 new("odeModel",
   main = function(time, state, parameters){
     with(as.list(c(state, parameters)),{
        sigma_t <- (1 - eps * cos(2 * pi * time/equinox))/(1 + eps)
        
        dA <- q * sigma_t * r * A * (1 - A/(sigma_t * K)) -
              q * sigma_t * g * Z * A/(A + h_A) +
              d * (sigma_t * K - A)
               
        dZ <- q * sigma_t * e * g * Z * A/(A + h_A) -
              q * sigma_t * m * Z -
              q * sigma_t * sigma_t * F * Z^2 / (Z^2 + h_Z^2)

        # the output, packed as a list
        list(c(dA, dZ), c(sigma_t = sigma_t, q=q))
      })
    },

    parms = c(
      r    = 0.5,    # (day^-1) maximum algal growth rate
      K    = 10,     # (mg DWT L^-1) algal carrying capacity
      g    = 0.4,    # (day^-1) maximum zooplankton grazing rate
      h_A  = 0.6,    # (mg DWT L^-1) half-saturation algal concentration for zooplankton grazing
      d    = 0.01,   # (—) inflow rate of algae from ungrazed parts
      e    = 0.6,    # (—) efficiency of food conversion to growth
      m    = 0.15,   # (day^-1) loss rate of zooplankton
      h_Z  = 0.5,    # (mg DWT L^-1) half-saturation zooplankton concentration for fish predation
      eps  = 0.7,    # (—) seasonal forcing strength
      F    = 0.0965, # (-) fish density
      q    = 0.812,  # (-) temperature coefficient
      equinox =  365 # 365.24218967  # length of a year
    ),
    times  = c(from=0, to=365, by=1),      # (day of the year) time
    init   =  c(
      A = 1,       # (mg L^-1) Biomass of Algae
      Z = 0.5      # (mg L^-1) Biomass of Zooplankton
    ),
    solver = "lsoda"

  )
}


