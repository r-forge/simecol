## general solver for simObj models and analytically solved odeModel's
##
## NOTE 1: iteration returns the new state
##         (as opposed to ODE solvers which return the first derivatives)
## NOTE 2: the special parameter DELTAT is set here to ensure
##         consistency with the times vector.

setGeneric("iteration",
  function(y, times=FALSE, func=FALSE, parms=FALSE, animate=FALSE, ...)
  standardGeneric("iteration")
)

setMethod("iteration", "simObj",
  function(y, times=NULL, func=NULL, parms=NULL, animate=FALSE, ...) {
    init              <- y@init
    times             <- fromtoby(y@times)
    func              <- y@main
    parms             <- y@parms
    inputs            <- y@inputs
    equations         <- y@equations
    environment(func) <- environment()
    #attach(equations)
    #on.exit(detach(equations))
    equations               <- addtoenv(equations)
    parms$DELTAT <- 0
    out <- list(func(times[1], init, parms))
    for (i in 2:length(times)) {
      time <- times[i]
      parms$DELTAT <- times[i] - times[i-1]
      init <- func(time, init, parms)
      out  <- c(out, list(init))
      if (animate) {
        y@out   <- out
        plot(y, index=i, ...)
      }
    }
    out
  }
)

setMethod("iteration", "odeModel",
  function(y, times=NULL, func=NULL, parms=NULL, animate=FALSE, ...) {
    init              <- y@init
    times             <- fromtoby(y@times)
    func              <- y@main
    parms             <- y@parms
    inputs            <- y@inputs
    equations         <- y@equations
    environment(func) <- environment()
    attach(equations)
    on.exit(detach(equations))
    n   <- length(init)
    parms <- c(parms, DELTAT = 0)
    nm  <- c("time", if (!is.null(attr(init, "names")))
             names(init) else as.character(1:n))
    out <- unlist(func(times[1], init, parms))
    for (i in 2:length(times)) {
      time <- times[i]
      parms["DELTAT"] <- times[i] - times[i - (i>1)] # is zero if i=1
      init <- unlist(func(time, init, parms))
      out  <- rbind(out, init)
      if (animate) {
        y@out   <- out
        plot(y, index=i, ...)
      }
    }
    row.names(out) <- NULL
    out <- as.data.frame(cbind(times, out))
    names(out) <- nm
    out
  }
)
