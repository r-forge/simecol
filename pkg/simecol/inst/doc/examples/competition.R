competition <- new("odeModel",
  main = function (time, init, parms) {
    with(as.list(c(init, parms)), {
      dS  <- Sin - mu1 * S * X1 - mu2 * S * X2
      dX1 <- mu1 * S * X1 - d1 * X1
      dX2 <- mu2 * S * X2 - d2 * X2
      list(c(dS, dX1, dX2))
    })
  },
  parms  = c(Sin = 1, mu1=0.21, mu2=0.1, d1=0.2, d2 = 0.1),
  times  = c(from = 0, to = 100, by = 0.5),
  init   = c(S=1, X1=0.1, X2=0.1),
  solver = "lsoda"
)

#competition <- sim(competition)
#plot(competition)
