library(simecol)
#library(ddesolve)
source("dde_workaround.R")

### simple model without nested sub-equations ##################################

data(lv)

### workaround on "model level"
#main(lv) <- function (time, init, parms) {
#              x <- init
#              p <- parms
#              dx1 <-   p["k1"] * x[1] - p["k2"] * x[1] * x[2]
#              dx2 <- - p["k3"] * x[2] + p["k2"] * x[1] * x[2]
#              list(c(dx1, dx2),0)
#              #c(dx1, dx2)
#            }


times(lv) <- c(from=0, to=1000, by=1)

solver(lv) <- "lsoda"
system.time(
lv <- sim(lv, rtol=1e-6, atol=1e-6)
)


solver(lv) <- "dde"
system.time(
lv <- sim(lv, tol=1e-6)
)

plot(lv)

### complex model with nested sub-equations ####################################

data(upca)

solver(upca) <- "lsoda"
system.time(
  plot(sim(upca, rtol=1e-6, atol=1e-6))
)

solver(upca) <- "dde"
system.time(
  plot(sim(upca, tol=1e-6))
)
