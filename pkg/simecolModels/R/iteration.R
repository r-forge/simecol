setMethod("iteration", "indbasedModel",
  function(y, times=NULL, func=NULL, parms=NULL,
                          animate=FALSE, ...) {
    observer = function(init, time, i, out, y){
      if (is.null(y@observer)) {
        ## default: simply return the state
        init 
      } else {
        ## call a function provided by the observer slot of the simObj
        if (length(formals(y@observer)) == 1) {
          y@observer(init)                    # for compatibility
        } else {
          y@observer(init, time, i, out, y)   # experimental
        }
      }
    }
    init              <- y@init
    times             <- fromtoby(y@times)
    func              <- y@main
    parms             <- y@parms
    inputs            <- y@inputs
    equations         <- y@equations
    environment(func) <- environment()
    equations         <- addtoenv(equations)
    parms$DELTAT <- 0
    res <- observer(init, times[1], 1, NULL, y)
    out <- res
    for (i in 2:length(times)) {
      time <- times[i]
      parms$DELTAT <- times[i] - times[i-1]
      init <- func(time, init, parms)
      res  <- observer(init, time, i, out, y)
      if (is.vector(res)) {
        out  <- rbind(out, res, deparse.level=0)
      } else {
        out  <- c(out, list(res))
      }
    }
    if(is.vector(res)) {
      # row.names(out) <- NULL ## now obsolete, see deparse.level=0
      out <- cbind(times, out)
      out <- as.data.frame(out)
    } else {
      out
    }
  }
)
