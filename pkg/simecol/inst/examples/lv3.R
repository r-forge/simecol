##########################################
# Lotka-Volterra System with 3 equations
#   and external ressource (S.in)
##########################################

lv3<- new("odeModel",
  main = function(time, init, parms, ...) {
    x <- init
    p <- parms

    s <- x[1] # substrate
    p <- x[2] # producer
    k <- x[3] # consumer
    input <- approxTime1(inputs, time, rule=2)
    with(as.list(parms),{
      s.in <- input["s.in"]
      ds <- s.in  - b*s*p + g*k
      dp <- c*s*p - d*k*p
      dk <- e*p*k - f*k
      res<-c(ds, dp, dk)
      list(res)
    })
  },
  parms = c(b=0.1, c=0.1, d=0.1, e=0.1, f=0.1, g=0),
  times  = c(from=0, to=200, by=1),
  inputs = as.matrix(
    data.frame(
      time = c(0,   99, 100,  101, 200),
      s.in = c(0.1, 0.1, 0.5, 0.1, 0.1)
    )
  ),
  init = c(s=1, p=1, k=1),
  solver = "rk4" #"lsoda"
)

