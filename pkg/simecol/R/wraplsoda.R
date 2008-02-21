## workaround for compatibility with lsoda
## may be obsolete in the near future

wraplsoda <- function(obj, ...) {
  if (!is(obj, "odeModel")) stop("wraplsoda requires argument of class odeModel")
  inputs     <- obj@inputs
  init       <- obj@init
  times      <- fromtoby(obj@times)
  func       <- obj@main
  equations  <- obj@equations
  parms      <- obj@parms
  inputs     <- obj@inputs
  environment(func) <- environment()
  #attach(equations)
  #on.exit(detach(equations))
  equations               <- addtoenv(equations)
  lsoda(init, times, func, parms, ...)
}

