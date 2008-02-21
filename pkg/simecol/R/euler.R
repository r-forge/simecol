## Euler integration with fixed-step-integration
## R-Implementation by Th. Petzoldt,
## using some code from Woodrow Setzer

setGeneric("euler", function(y, times, func, parms, ...) standardGeneric("euler"))

setMethod("euler", "numeric",
  function(y, times, func, parms, ...) {
    if (!is.numeric(y))     stop("`y' must be numeric")
    if (!is.numeric(times)) stop("`times' must be numeric")
    if (!is.function(func)) stop("`func' must be a function")
    # if (!is.numeric(parms)) stop("`parms' must be numeric")
    n <- length(y)
    ## Call func once to figure out whether and how many "global"
    ## results it wants to return and some other safety checks
    rho <- environment(func)
    tmp <- eval(func(times[1],y,parms),rho)
    if (!is.list(tmp)) stop("Model function must return a list\n")
    if (length(tmp[[1]]) != length(y))
      stop(paste("The number of derivatives returned by func() (",
                 length(tmp[[1]]),
                 "must equal the length of the initial conditions vector (",
                 length(y),")",sep=""))
    Nglobal <- if (length(tmp) > 1) length(tmp[[2]]) else 0
    y0 <- y
    out <- c(times[1], y0)
    for (i in 1:(length(times)-1)) {
        t  <- times[i]
        dt <- times[i+1] - times[i]
        dy <- dt * func(t, y0, parms)[[1]]
        y1 <- y0 + dy
        out<- rbind(out, c(times[i+1], y1))
        y0 <- y1
    }
    nm <- c("time", if (!is.null(attr(y, "names")))
            names(y) else as.character(1:n))
    if (Nglobal > 0) {
        out2 <- matrix(nrow=nrow(out), ncol=Nglobal)
        for (i in 1:nrow(out2))
            out2[i,] <- func(out[i,1], out[i,-1], parms)[[2]]
        out <- cbind(out, out2)
        nm <- c(nm,
                if (!is.null(attr(tmp[[2]],"names"))) names(tmp[[2]])
                else as.character((n+1) : (n + Nglobal)))
    }
    dimnames(out) <- list(NULL, nm)
    out
  }
)

setMethod("euler", "odeModel",
  function(y, times=NULL, func=NULL, parms=NULL) {
    times             <- fromtoby(y@times)
    func              <- y@main
    inputs            <- y@inputs
    equations         <- y@equations
    environment(func) <- environment()
    #attach(equations)
    #on.exit(detach(equations))
    equations               <- addtoenv(equations)
    euler(y@init, times, func, y@parms)
  }
)
