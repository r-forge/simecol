## plotting methods for simObj'ects

setMethod("plot", c("simObj", "missing"),
  function(x, y, ...) {
    warning("No default plot method available for this class.\n",
    "  Please write your own plot method\n",
    "  or extract output data and use standard routines.")
  }
)

setMethod("plot", c("odeModel", "missing"),
  function(x, y, ...) {
  	if (is.null(x@out)) stop("Please simulate the model before plotting")
    oldpar <- par(no.readonly=TRUE)
    on.exit(par(oldpar))
  	out    <- as.data.frame(x@out)
    nstates <- ncol(out) - 1
    ## one figure per page if nstates = 1
    ## two figures if nstates = 2
    ## four figures if nstates > 2
    par(mfrow=c(1 + (nstates > 1), 1 + (nstates > 2)))
    nam <- names(out)
    for (i in 1:nstates) {
      graphics:::plot(out[[1]], out[[i+1]],
                      type="l", xlab=nam[1], ylab=nam[i+1], ...)
      if ((i %%4) ==0  & nstates > i) readline("press return for next page")
    }
  }
)

setMethod("plot", c("gridModel", "missing"),
  function(x, y, index=1:length(x@out), delay=0, ...) {
   	if (is.null(x@out)) stop("Please simulate the model before plotting")
    oldpar <- par(no.readonly=TRUE)
    on.exit(par(oldpar))
    for (i in index) {
      image(x@out[[i]], main=i, ...)
      Sys.sleep(0.001 * delay)
    }
  }
)

setMethod("plot", c("rwalkModel", "missing"),
  function(x, y, index=1:length(x@out), delay=0, ...) {
   	if (is.null(x@out)) stop("Please simulate the model before plotting")
    oldpar <- par(no.readonly=TRUE)
    on.exit(par(oldpar))
    for (i in index) {
      dat <- x@out[[i]]
      if (is.matrix(dat)) dat <- as.data.frame(dat)
      graphics:::plot(dat$x, dat$y,
                      xlim = x@parms$area[c(1,2)],
                      ylim = x@parms$area[c(3,4)],
                      xlab="x", ylab="y", main=i, ...)
      Sys.sleep(0.001 * delay)
    }
  }
)

