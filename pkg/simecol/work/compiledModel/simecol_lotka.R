## simecol - object with compiled code

library("simecol")
library("deSolve") # load this as second package to mask lsoda from odesolve

# compile C++ code within R
# (requires installed compiler)
# on Windows: http://www.murdoch-sutherland.com/Rtools/
system("R CMD SHLIB lotka.cpp")


modeldll <- dyn.load("lotka.dll")

clotka <- new("odeModel",
  main = function(time, init, parms) {
     # list with dllname, func, nout, [jacfunc]
     list(lib     = "lotka",
          func    = "dlotka",
          jacfunc = NULL,
          nout    = 2)
  },
  parms  = c(k1=0.2, k2=0.2, k3=0.2),
  times  = c(from=0, to=100, by=0.5),
  init   = c(prey=0.5, predator=1),
  solver = "clsoda", # rename this
  initfunc = function(obj) {
    obj
  }
)

clsoda <- function(init, times, func, parms, ...) {
  f <- func()
    as.data.frame(lsoda(init, times, func=f$func,
      parms = parms, dllname = f$lib, jacfunc=f$jacfunc, nout = f$nout, ...)
    )
}

clotka <- sim(clotka)

plot(clotka)

times(clotka)["to"] <- 1000
plot(sim(clotka))

plot(sim(clotka, atol=1)) # accuracy too low, for testing only

dyn.unload(as.character(modeldll[2]))