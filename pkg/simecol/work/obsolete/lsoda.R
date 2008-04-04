setGeneric("lsoda")

setMethod("lsoda", "odeModel",
  function(y, times=NULL, func=NULL, parms=NULL,
  rtol = "missing", atol = "missing", tcrit = "missing",
  jacfunc = "missing", verbose = "missing",
  dllname = "missing", hmin = "missing", hmax = "missing", ...) {
    times             <- fromtoby(y@times)
    func              <- y@main
    inputs            <- y@inputs
    equations         <- y@equations
    parms <- y@parms
    environment(func) <- environment()
    equations               <- addtoenv(equations)
    lsoda(y@init, times, func, parms, ...)
  }
)

