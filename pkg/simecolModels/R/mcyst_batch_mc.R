
mcyst_batch_mc <- function(){
  new("odeModel",
    main = function (time, init, parms, ...) {
      x <- init
      signal <- approxTime1(inputs, time, rule = 2)[c("p", "dM")]
  
      with(as.list(parms),{
        dx1 <- mu * x[1] * (1-x[1]/K)
        p   <- signal["p"]
        dM  <- signal["dM"]
        dx2 <- p * mu * (1-x[1]/K) * x[1] - dM * x[2]
        list(c(dx1, dx2))
      })
    },
    parms  = c(
      mu = 0.3,      # Growth rate (1/d)
      K  = 1e7,      # Carrying Capacity (cells /L)
      #dM = 0.026,   # MCYST decay rate (1/d)
      dMmin = 0,
      dMmax = 0.05,
      dMrho = 0,     # autocorrelation coefficient
      #fp = 100,     # MCYST production coefficient (fg/cell)
      pmin = 67,
      pmax = 133,
      prho = 0      # autocorrelation coefficient
    ),
    init = c(
      cells = 1e5,   # Inoculum    (cells/L)
      mcyst = 91.5e5 # Start-MCYST (fg/L)
    ),
    times  = c(from=0, to=40, by=0.5),
    solver = "rk4",
    initfunc = function(obj) {
       with(as.list(parms(obj)), {
         x  <- fromtoby(times(obj))
         p  <- pcuseries(length(x), rho=prho,  min=pmin,  max=pmax)
         dM <- pcuseries(length(x), rho=dMrho, min=dMmin, max=dMmax)
         inputs(obj) <- as.matrix(data.frame(x=x, p=p, dM=dM))
         obj
       })
    }
  )
}
