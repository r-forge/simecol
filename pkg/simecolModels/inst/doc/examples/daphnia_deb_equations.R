daphnia_deb_equations <- list(
    ## length to weight conversion
    length2weight = function(length, l2w){
        l2w * length^3
    },
    
    ## weight to length conversion
    weight2length = function(weight, l2w) {
        (weight/l2w)^(1/3)    
    },

    ## temperature dependent egg development time
    bottrell = function(temp, p, t.kelvin=273.15){
      with(p, {
        a <- 3.3956        
        b <- 0.2193
        c <- -0.3414        
        exp(a + b*log(temp-t.kelvin) + c*log(temp-t.kelvin)^2)
      })
    },

    ## Holling Type II functional response
    holling2 = function(ks, s) s/(ks + s),

    ## Arrhenius temperature correction
    arrheniuscorr = function(t.kelvin, t.arrh) {
      exp(t.arrh/293.15 - t.arrh/t.kelvin)
    },
    
    ## absolute ingestion rate
    ingestion = function(food, t.kelvin, p) {
      with(p, {
        holling2(ing.ks, food) * ping * 
          arrheniuscorr(t.kelvin, t.arrh)
      })
    },
     
    ## weight-specific maintenance rate
    respiration = function(food, t.kelvin, p) {
      with(p,{
         (resp.min + (resp.max - resp.min) *
          holling2(ing.ks, food)) *
          arrheniuscorr(t.kelvin, t.arrh) 
      })
    },
    
    ## food-dependent assimilation efficiency 
    ass.eff = function(food, p) {
      with(p, {
        ass.eff.max - (ass.eff.max - ass.eff.min) *  holling2(ing.ks, food)
      })
    },
  
    vonBertWmax = function(food, p) {
      with(p,{
        func.response <-  holling2(ing.ks, food)
        w.max <- (kappa * ass.eff(food, p)*func.response*ping/
                  (resp.min+(resp.max-resp.min)*func.response))^3
        w.max
      })
    },
    
    ## maximal body length
    vonBertLmax = function(food, p) {
      with(p,{
        func.response <-  holling2(ing.ks, food)
        l.max <- (kappa * ass.eff(food, p)*func.response*ping / 
                    (resp.min+(resp.max-resp.min)*func.response)) / (l2w^(1/3))
        l.max
      })
    },

    #von Bertalanffy coefficient k
    vonBertK = function(food, maxlength, startlength, kelvin, p) {
      with(p,{
        w0 <- l2w*startlength^3 #weight of neonates
        func.response <-  holling2(ing.ks, food)
        dW.dt <- (kappa * ass.eff(food, p) * ping * func.response * w0^(2/3) - 
                    w0 * (resp.min+(resp.max-resp.min)*func.response)  ) * 
                    exp(t.arrh/293 - t.arrh/kelvin) #dW/dt at age=0
        k <- dW.dt/(3*l2w*startlength^2*(maxlength-startlength))
        k
      })
    }
)

## global dummy functions
length2weight <- weight2length <- bottrell <- holling2 <- 
  arrheniuscorr <- ingestion <- respiration <- ass.eff <-
  vonBertWmax <- vonBertLmax <- vonBertK <- 
  function(...) stop("daphnia_equations not correctly assigned")
