## experimental code !!!
## using imported function from package ddesolve

if(require("ddesolve", quietly=TRUE)) {

  setGeneric("dde")
  
  setMethod("dde", "odeModel", 
    function(y, times = NULL, func = NULL, parms = NULL, 
      switchfunc = NULL, mapfunc = NULL, tol = 1e-08, dt = 0.1, hbsize = 10000) {
      if (!require("ddesolve")) stop("package ddesolve is not installed")
      times             <- fromtoby(y@times)
      func              <- y@main
      inputs            <- y@inputs
      equations         <- y@equations
      parms             <- y@parms
  
      environment(func) <- environment()
      equations         <- addtoenv(equations)
      
      ## 1) fix parameter list (dde does not allow ...)
      ## 2) fix return value (dde does not allow one-element list)
      genfunc <- function() {
        function(time, init, parms) {
           ret <- func(time, init, parms)
           if (is.list(ret) & length(ret) == 1) ret <- unlist(ret)
           ret
        }
      }
      func2 <- genfunc()
     
      o <- dde(y@init, times, func2, parms, switchfunc, mapfunc, tol, dt, hbsize)
      names(o)[1] <- "time"  # 3) fix output of dde, named "t" and not "time"
      o
    }
  )
}