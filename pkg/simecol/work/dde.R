## experimental code !!!
## using imported function from package ddesolve

setGeneric("dde")

setMethod("dde", "odeModel",
  function(y, times=NULL, func=NULL, parms=NULL,
    switchfunc=NULL, mapfunc=NULL,
    tol=1e-08, dt=0.1, hbsize=10000) {

    times             <- fromtoby(y@times)
    func              <- y@main
    inputs            <- y@inputs
    equations         <- y@equations
    parms             <- y@parms

    environment(func) <- environment()
    equations         <- addtoenv(equations)

    o <- dde(y@init, times, func, parms, switchfunc, mapfunc, tol, dt, hbsize)
    names(o)[1] <- "time"  ## fix output of dde, named "t" and not "time"
    o
  }
)
