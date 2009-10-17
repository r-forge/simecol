daphnia_deb_lifeequations <-list(
  ## list to vector
  list2vec = function(x){
    as.vector(unlist(x))
  },

  ## vector to list
  vec2list = function(vec){
    X <- vec[1:3]
    Z <- as.data.frame(matrix(vec[4:length(vec)], ncol=6))
    names(X)    <- c("x1", "x2", "x3")
    colnames(Z) <- c("age", "weight", "egg", "eggage", "length", "eggstorage")
    list(X=X, Z=Z)
  },

  ## Growth of Phytoplankton
  wx = function(mu, K, X, sumX, t.kelvin, t.arrh){
    mu * X * (K - sumX)/K #* arrheniuscorr(t.kelvin, t.arrh)
  },

  ## Template for neonate
  newdaphnia = function(w=p$WON, l=p$SON,n) {
   if (n>=1) {
     data.frame(age = rep(0, n), weight = w, egg = 0, eggage = 0, length = l, eggstorage = 0)
   } else {NULL}
  },

  ## living
  live = function(model){
    out <- as.vector(out(sim(model), last=TRUE))[-1]
    vec2list(out)
  },

  ## reference system
  rescale = function(inds, refVol, samplesize){
    lowerscale <- samplesize[1]
    upperscale <- samplesize[2]
    if(nrow(inds) < lowerscale){
      scalefactor <- floor(upperscale / nrow(inds))
      refVol      <- refVol * scalefactor
      newpopulation <- inds
      for(i in 2:scalefactor){
        inds <- rbind(inds, newpopulation)
      }
    }
    if(nrow(inds) > upperscale){
      scalefactor <- nrow(inds) / lowerscale
      downscale   <- runif(nrow(inds))
      inds <- subset(inds, downscale < 1/scalefactor)
      refVol      <- refVol/scalefactor
    }
    list(refVol=refVol, Z=inds)
  },


  ## hatching
  hatch = function(inds, parms){
    with(parms, {
      # vector of eggs to be released
      eggs <- ifelse(inds$eggage > 1, inds$eggstorage, 0)
      eggs.release <- floor(eggs)
      # rest of eggs
      eggs.keep    <- eggs - eggs.release
      # after hatching, new eggs are laid to brood chamber
      inds$eggstorage <- ifelse(inds$eggage > 1,
                                       inds$egg,
                                       inds$eggstorage
                                )
      # resetting eggs and eggage if hatching took place
      inds$egg <- ifelse(inds$eggage > 1,
                                eggs.keep,
                                inds$egg
                         )
      inds$eggage <- ifelse(inds$eggage > 1,
                                0,
                                inds$eggage
                            )
      n.neonates <- sum(eggs.release)
      rbind(inds, newdaphnia(WON, SON, n.neonates))
    })
  },

  ## hatch ohne Rückhalt der Eier
  hatch.simple = function(inds){
    # Vektor mit zu entlassenden Eiern
    eggs <- ifelse(inds$eggage > 1, inds$egg, 0)
    eggs.release <- floor(eggs)
    # Rest der nicht ganzahligen Eier
    eggs.keep    <- eggs - eggs.release
    # nach hatch werden neue Eier und Eialter rückgesetzt
    inds$egg <- ifelse(inds$eggage > 1,
                              eggs.keep,
                              inds$egg
                       )
    inds$eggage <- ifelse(inds$eggage > 1,
                              0,
                              inds$eggage
                          )
    n.neonates <- sum(eggs.release)
    rbind(inds, newdaphnia(p$WON,p$SON,n.neonates))
  },

  ## survive
  survive = function(inds, parms){
    with(as.list(parms),{
      opt.weight  <- length2weight(inds$length, l2w)
      ind.mort    <- ifelse(inds$weight < critical.weight * opt.weight,   #strong starvation?
                                           yes = starve.mort + mort, no = mort)
      ind.mort    <- ind.mort * DELTAT
      ind.dead    <- runif(length(ind.mort))
      survivors   <- subset(inds, ind.dead > -ind.mort & inds$age < life.span)
      if (nrow(survivors) >= 1) {survivors} else {inds}
    })
  }
)

## global dummy functions
list2vec <- vec2list <- wx <- newdaphnia <-  
  live <- rescale <- hatch <- hatch.simple <- survive <-
  function(...) stop("daphnia_lifeequations not correctly assigned")

