setGeneric("sim", function(obj, ...) standardGeneric("sim"))

setMethod("sim", "simObj",
  function(obj, ...) {
    out <- do.call(obj@solver, list(obj, ...))
    obj@out <- out
    invisible(obj)
  }
)

#setMethod("sim", "odeModel",
#  function(obj, ...) {
#    times <- fromtoby(obj@times)
#    #if (obj@solver == "lsoda") {
#    #  out <- wraplsoda(obj, ...)
#    #} else {
#      out <- do.call(obj@solver, list(obj, ...))
#    #}
#    obj@out <- as.data.frame(out)
#    invisible(obj)
#  }
#)

setMethod("sim", "odeModel",
  function(obj, ...) {
    times             <- fromtoby(obj@times)
    func              <- obj@main
    inputs            <- obj@inputs
    equations         <- obj@equations
    #parms             <- obj@parms
    environment(func) <- environment()

    equations        <- addtoenv(equations)
    out <- do.call(obj@solver, list(obj@init, times, func, obj@parms, ...))
    
    #out <- do.call(obj@solver, list(obj, ...))
    obj@out <- as.data.frame(out)
    invisible(obj)
  }
)



setMethod("sim", "gridModel",
  function(obj, ...) {
    times <- fromtoby(obj@times)
    out <- do.call(obj@solver, list(obj, ...))
    obj@out <- out
    invisible(obj)
  }
)

