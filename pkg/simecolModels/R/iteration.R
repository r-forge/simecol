setMethod("iteration", "indbasedModel",
  function(y, times=NULL, func=NULL, parms=NULL,
                          animate=FALSE, ...) {
    observer = function(init){
      if (is.null(y@observer)) init else y@observer(init)
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
    res <- observer(init)
    out <- res
    for (i in 2:length(times)) {
      time <- times[i]
      parms$DELTAT <- times[i] - times[i-1]
      init <- func(time, init, parms)
      res  <- observer(init)
      if (is.vector(res)) {
        out  <- rbind(out, res)
      } else {
        out  <- c(out, list(res))
      }
    }
    if(is.vector(res)) {
      row.names(out) <- NULL
      out <- cbind(times, out)
      out <- as.data.frame(out)
    } else {
      out
    }
  }
)
