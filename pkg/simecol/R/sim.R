setGeneric("sim", function(obj, initialize = TRUE, ...) standardGeneric("sim"))

setMethod("sim", "simObj",
  function(obj, initialize = TRUE, ...) {
    if (initialize & !is.null(obj@initfunc)) obj <- initialize(obj)
    out <- do.call(obj@solver, list(obj, ...))
    obj@out <- out
    invisible(obj)
  }
)

setMethod("sim", "odeModel",
  function(obj, initialize = TRUE, ...) {
    if (initialize & !is.null(obj@initfunc)) obj <- initialize(obj)
    times             <- fromtoby(obj@times)
    func              <- obj@main
    inputs            <- obj@inputs
    equations         <- obj@equations
    environment(func) <- environment()

    equations        <- addtoenv(equations)
    out <- do.call(obj@solver, list(obj@init, times, func, obj@parms, ...))
    #obj@out <- as.data.frame(out)
    obj@out <- out
    invisible(obj)
  }
)

setMethod("sim", "gridModel",
  function(obj, initialize = TRUE, ...) {
    if (initialize & !is.null(obj@initfunc)) obj <- initialize(obj)
    times <- fromtoby(obj@times)
    out <- do.call(obj@solver, list(obj, ...))
    obj@out <- out
    invisible(obj)
  }
)

