`fitOdeModel` <-
function(simObj, whichpar=names(parms(simObj)), 
  obstime, yobs, 
  sd.yobs = as.numeric(lapply(yobs, sd)),
  initialize = TRUE, debuglevel = 0, 
  fn = ssqOdeModel,  
  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"),
  lower = -Inf, upper = Inf, control = list(), ...)  {

   
  par <- parms(simObj)[whichpar]
  m <- optim(par, fn = fn, simObj = simObj, obstime = obstime,
           yobs = yobs, sd.obs = sd.yobs, initialize = initialize,
           debuglevel = debuglevel,
           method = method,
           lower = lower, upper = upper,
           control = control, ...)
  m 
}

