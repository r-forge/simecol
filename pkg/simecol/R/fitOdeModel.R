

`fitOdeModel` <-
function(simObj, whichpar=names(parms(simObj)), 
  obstime, yobs, 
  sd.yobs = as.numeric(lapply(yobs, sd)),
  initialize = TRUE, 
  debuglevel = 0, 
  fn = ssqOdeModel,  
  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "PORT"),
  lower = -Inf, upper = Inf, control = list(), ...)  {

  method <- match.arg(method)
   
  par <- parms(simObj)[whichpar]
  if (!(min(lower) == -Inf | max(upper) == Inf)) {
    lower <- lower[whichpar]
    upper <- upper[whichpar]
  }
  upper. <-  Inf
  lower. <- -Inf
  if (!(method %in% c("L-BFGS-B", "PORT"))) {
    upper. <- upper
    lower. <- lower 
    upper <-   Inf
    lower <- - Inf
  }
  
  par <- p.unconstrain(par, lower., upper.)

  if (method == "PORT") {
    m <- nlminb(start=par, objective = fn, #gradient = NULL, hessian = NULL,
             simObj = simObj, obstime = obstime,
             yobs = yobs, sd.yobs = sd.yobs,
             pnames = names(par),  # !!! workaround as nlminb does not pass the names
             initialize = initialize,
             debuglevel = debuglevel,
             scale = 1, control = control)#, lower = lower, upper = upper)
  } else {
    m <- optim(par, fn = fn, simObj = simObj, obstime = obstime,
             yobs = yobs, sd.yobs = sd.yobs, initialize = initialize,
             lower. = lower.,
             upper. = upper.,
             debuglevel = debuglevel,
             method = method,
             lower = lower, upper = upper,
             control = control, ...)
  }
  cat(m$message, "\n")
  m$par <- p.constrain(m$par, lower., upper.)           
  m 
}

