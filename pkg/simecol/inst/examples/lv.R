########################################################################
## Predator-Prey Model
########################################################################

lv <- new("odeModel", 
  main = function (time, init, parms) {
    with(as.list(c(init, parms)), {
      dx1 <-   k1 * N1 - k2 * N1 * N2
      dx2 <- - k3 * N2 + k2 * N1 * N2
      list(c(dx1, dx2))
    })
  },
  parms  = c(k1 = 0.2, k2 = 0.2, k3 = 0.2),
  times  = c(from = 0, to = 100, by = 0.5),
  init   = c(N1 = 0.5, N2 = 1),
  solver = "lsoda"
)

lv <- sim(lv)
plot(lv)

